module IR where

import Prelude hiding (seq, EQ, LT, GT)
import Data.Map as Map
import qualified AST
import Typing (Env)
import Combinators2

type Label = String
data BinOp = PLUS | MINUS | MUL | DIV | AND | OR | MOD | LSHIFT | RSHIFT | ARSHIFT | XOR deriving (Show, Eq)
data RelOp = EQ | NE| LT | GT| LE | GE | ULT | ULE | UGT | UGE deriving (Show, Eq)
data Exp = CONST Int | NAME String | TEMP Label | BINOP BinOp Exp Exp | MEM Exp | CALL Exp [Exp] | ESEQ Stm Exp deriving Show
-- TODO: what to do with the [Exp] in JUMP?
data Stm = MOVE Exp Exp | EXP Exp | JUMP Exp [Exp] | CJUMP RelOp Exp Exp Label Label | SEQ Stm Stm | LABEL String deriving Show
data IR = Ex Exp | Nx Stm | Cx (Label -> Label -> Stm)

instance Show IR where
	show (Ex e) = show e
	show (Nx s) = show s
	show (Cx f) = error "FAIL" -- FIXME: cannot show function 

data Frame = Frame {curPos :: Int, curArgPos :: Int, id :: String, varMap :: Map Label Int, body :: IR} deriving Show
data Reg = Reg { labelCount :: Int, regCount :: Int, curFrame :: Frame, frameList :: Map Label Frame} deriving Show
type F a = M a Reg

newLabel :: F Label
newLabel r@(Reg {labelCount=l}) = ("l" ++ show l, r{labelCount=l+1})

newReg :: F Label
newReg r@(Reg {regCount=l}) = ("l" ++ show l, r{regCount=l+1})

addVarToFrame :: Label -> F Int
addVarToFrame l r@(Reg {curFrame=f}) = case f of
	(Frame {curPos=p,varMap=m} ) -> (p, r{curFrame=f{curPos=p + 1, varMap= Map.insert l p m}})
	
-- FIXME: partially move to SSM
addArgToFrame :: Label -> F Int
addArgToFrame l r@(Reg {curFrame=f}) = case f of
	(Frame {curArgPos=p,varMap=m} ) -> (p, r{curFrame=f{curArgPos= p - 1, varMap= Map.insert l p m}})

-- TODO: move to SSM
getVarLoc :: Label -> F Int
getVarLoc l r@(Reg {curFrame=f}) = case f of
	(Frame {varMap=m} ) -> case Map.lookup l m of
		Just x -> yield x r
		Nothing -> error $ "Error: location of variable " ++ l ++ " not found in map"

addBody :: IR -> F IR
addBody b r@(Reg {curFrame=f}) = (b, r{curFrame=f{body=b}})

newFrame :: String -> F String
newFrame s r@(Reg {curFrame=f}) = (s, r{curFrame=(Frame 1 (-2) s Map.empty (Ex $ CONST 0))})

storeFrame :: String -> F Frame
storeFrame s r@(Reg {curFrame=f, frameList=fl}) = (f, r{frameList=Map.insert s f fl})

getFunctionName :: F String
getFunctionName r@(Reg {curFrame=f}) = case f of
	(Frame {IR.id=s}) -> (s, r)

-- make a sequence from list
seq :: [Stm] -> Stm
seq [] = error "Empty sequence"
seq [x] = x
seq (x:xs) = SEQ x (seq xs)

-- TODO: add special case for CONST 1, CONST 0 for Cx?
unEx :: IR -> F Exp
unEx (Ex e) = yield e
unEx (Nx s) = yield $ ESEQ s (CONST 0)
unEx (Cx c) = newReg # newLabel # newLabel >-> \((r,t),f) -> ESEQ (seq [(MOVE (TEMP r) (CONST 1)), (c t f), (LABEL f), (MOVE (TEMP r) (CONST 0)), (LABEL t)]) (TEMP r)

unNx :: IR -> F Stm
unNx (Ex e) = yield $ EXP e
unNx (Nx s) = yield s
unNx (Cx c) = newLabel >-> \a -> SEQ (c a a) (LABEL a)

unCx :: IR -> F (Label -> Label -> Stm)
unCx (Nx s) = error $ "Cannot convert Nx to Cx"
unCx (Ex e) = yield $ CJUMP EQ e (CONST 1)
unCx (Cx c) = yield c

convertProg :: AST.Prog -> Reg
convertProg p = snd $ (iter (\x -> convert x !++! unNx) p >-> (\x -> Nx (seq x))) (Reg 0 0 (Frame 1 (-2) "" Map.empty (Ex(CONST 0))) Map.empty)

class Convert a where
	convert :: a -> F IR

instance Convert AST.Exp where
	convert (AST.Int a)  = yield $ Ex $ CONST a
	convert (AST.Bool False) = yield $ Ex $ CONST 0
	convert (AST.Bool True) = yield $ Ex $ CONST 1
	convert (AST.Op1_ AST.UnitaryMinus a) = convert a !++! unEx >-> \k -> Ex $ BINOP MINUS (CONST 0) k
	convert (AST.Op1_ AST.Negate a) = convert a !++! unEx >-> \k -> Ex $ BINOP XOR (CONST 1) k

	convert AST.EmptyList = yield $ Ex $ CALL (TEMP "alloc") [CONST 0, CONST 0]
	convert (AST.ExpOp_ AST.AppCons a b) = (convert a !++! unEx # convert b !-+! unEx) >-> \(x,y) -> Ex $ CALL (TEMP "alloc") [x, y]

	--FIXME: check if arguments are function calls. If they are, extract	
	convert (AST.ExpOp_ o a b) = (convert a !++! unEx # convert b !-+! unEx) >-> \(l,r) -> if elem o relOp then Cx (CJUMP (getOp rel) l r) else Ex (BINOP (getOp bin) l r)
		where
		relOp = [AST.Equals, AST.Less, AST.More, AST.LessEq, AST.MoreEq, AST.NotEq]
		rel = [(AST.Equals, EQ), (AST.Less, LT), (AST.More, GT), (AST.LessEq, LE), (AST.MoreEq, GE), (AST.NotEq, NE)]
		bin =	[(AST.Add, PLUS), (AST.Sub, MINUS), (AST.And, AND), (AST.Or, OR), (AST.Mul, MUL), (AST.Div, DIV), (AST.Mod, MOD)]
		getOp m = case Prelude.lookup o m of
			Just a -> a
			Nothing -> error $ "Undefined operator " ++ show o
	--convert (AST.Tuple a b) =

	-- TODO: only looks up local function, expand?
	-- FIXME: what to do with names that overlap?
	--convert (AST.Id name) = yield $ Ex $ MEM (TEMP name)
	-- FIXME: always relative to MP
	convert (AST.Id name) =  getVarLoc name >-> \k -> Ex $ MEM ((CONST k)) -- (BINOP PLUS (TEMP "fp")
	convert (AST.FunCall (id, args)) = convert (AST.FunCall_ (id, args))

instance Convert AST.Stmt where
	convert (AST.If cond stmt) = convert cond !++! unCx # convert stmt !-+! unNx # newLabel # newLabel >-> \(((c,s),t),f) -> Nx $ seq [c t f, LABEL t, s ,LABEL f]
	convert (AST.IfElse cond stmt1 stmt2) = convert cond !++! unCx # convert stmt1 !-+! unNx # convert stmt2 !-+! unNx # newLabel # newLabel # newLabel >-> \(((((c,s),fb),t),f),d) -> Nx $ seq [c t f, LABEL t, s, JUMP (NAME d) [], LABEL f, fb, LABEL d]
	convert (AST.While cond stmt) = convert cond # convert stmt !++! uncurry parseWhile
		where
		parseWhile c s = newLabel # newLabel !++! \(b,d) -> (unCx c >-> \x -> (x b d, b, d)) # unNx s # newLabel  >-> \(((cc, b, d),ss),t) -> Nx (seq [LABEL t, cc, LABEL b, ss, JUMP (NAME t) [], LABEL d])
	--convert (AST.Assign id exp) = convert exp !++! unEx >-> \e -> Nx $ MOVE (MEM (TEMP id)) e
	convert (AST.Assign id exp) = getVarLoc id # convert exp !-+! unEx >-> \(k,e) -> Nx (MOVE (MEM (CONST k)) e) -- (BINOP PLUS (TEMP "fp")
	convert (AST.Seq stmt) = (iter (\x -> convert x !++! unNx) stmt) >-> \x -> Nx (seq x)
	convert (AST.FunCall_ (id, args)) = (iter (\x -> convert x !++! unEx) args) >-> \x -> Ex $ CALL (TEMP id) x
	convert (AST.Return (Just a)) = convert a !++! unEx # getFunctionName >-> \(e,n) -> Nx $ SEQ(MOVE (MEM (TEMP "_res")) e) (JUMP (NAME (n++"_end")) []) -- store result in specific temporary

instance Convert AST.VarDecl where
--	convert (AST.VD t id exp) = convert exp !++! unEx >-> \e -> Nx $ MOVE (MEM (TEMP id)) e
	convert (AST.VD t id exp) = addVarToFrame id # convert exp !++! \(k,e) -> unEx e >-> (\x -> (k,x)) >-> \(k,e) -> Nx $ MOVE (MEM (CONST k)) e -- (BINOP PLUS (TEMP "fp") )

-- Arguments are added first on stack
-- Function definitions are added next
instance Convert AST.FunDecl where
	convert (AST.FD t id args var stmt) = newFrame id >>| ((iter (addArgToFrame . snd) (reverse args) # listConv var # convert stmt !-+! unNx) >-> (\((a,v),s) -> Nx $ seq (v ++ [s] ++ [LABEL (id ++"_end")]))) !++! addBody >>- storeFrame id
		where
		listConv = iter (\x -> convert x !++! unNx)

instance Convert AST.Decl where
	convert (AST.VarDecl v) = convert v
	convert (AST.FunDecl f) = convert f

