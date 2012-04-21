module IR where

import Prelude hiding (seq, EQ, LT, GT)
import Data.Map as Map
import qualified AST
import Typing (Env)
import Combinators2

type Label = String
data BinOp = PLUS | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR deriving Show
data RelOp = EQ | NE| LT | GT| LE | GE | ULT | ULE | UGT | UGE deriving (Show, Eq)
data Exp = CONST Int | NAME String | TEMP Label | BINOP BinOp Exp Exp | MEM Exp | CALL Exp [Exp] | ESEQ Stm Exp deriving Show
-- TODO: what to do with the [Exp] in JUMP?
data Stm = MOVE Exp Exp | EXP Exp | JUMP Exp [Exp] | CJUMP RelOp Exp Exp Label Label | SEQ Stm Stm | LABEL String deriving Show
data IR = Ex Exp | Nx Stm | Cx (Label -> Label -> Stm)

instance Show IR where
	show (Ex e) = show e
	show (Nx s) = show s
	show (Cx f) = error "FAIL" -- FIXME: cannot show function 

data Frame = Frame {curPos :: Int, varMap :: Map Label Int, wordSize :: Int} deriving Show
data Reg = Reg { labelCount :: Int, regCount :: Int, curFrame :: Frame} deriving Show
type F a = M a Reg

newLabel :: F Label
newLabel r@(Reg {labelCount=l}) = ("l" ++ show l, r{labelCount=l+1})

newReg :: F Label
newReg r@(Reg {regCount=l}) = ("l" ++ show l, r{regCount=l+1})

addVarToFrame :: Label -> F Int
addVarToFrame l r@(Reg {curFrame=f}) = case f of
	(Frame {curPos=p,varMap=m} ) -> (p, r{curFrame=f{curPos=p+1, varMap= Map.insert l p m}})

getVarLoc :: Label -> F Int
getVarLoc l r@(Reg {curFrame=f}) = case f of
	(Frame {varMap=m} ) -> case Map.lookup l m of
		Just x -> yield x r
		Nothing -> error $ "Error: location of variable " ++ l ++ " not found in map"

-- make a sequence from list
seq :: [Stm] -> Stm
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

class Convert a where
	convert :: a -> F IR

instance Convert AST.Exp where
	convert (AST.Int a)  = yield $ Ex $ CONST a
	convert (AST.Bool False) = yield $ Ex $ CONST 0
	convert (AST.Bool True) = yield $ Ex $ CONST 1
	convert (AST.Op1_ AST.UnitaryMinus a) = convert a !++! unEx >-> \k -> Ex $ BINOP MINUS (CONST 0) k
	convert (AST.Op1_ AST.Negate a) = convert a !++! unEx >-> \k -> Ex $ BINOP XOR (CONST 1) k
	convert (AST.ExpOp_ o a b) = (convert a !++! unEx # convert b !-+! unEx) >-> \(l,r) -> if elem (getOp rel) relOp then Cx (CJUMP (getOp rel) l r) else Ex (BINOP (getOp bin) l r)
		where
		relOp = [EQ, NE, LT, GT, LE, GE, ULT, ULE, UGT, UGE]
		rel = [(AST.Equals, EQ), (AST.Less, LT), (AST.More, GT), (AST.LessEq, LE), (AST.MoreEq, GE), (AST.NotEq, NE)]
		bin =	[(AST.Add, PLUS), (AST.Sub, MINUS), (AST.And, AND), (AST.Or, OR), (AST.Mul, MUL), (AST.Div, DIV)]
		getOp m = case Prelude.lookup o m of
			Just a -> a
			Nothing -> error $ "Undefined operator"
	
	{-convert EmptyList =
	convert (ExpOp_ AppCons a EmptyList) = 
	convert (ExpOp_ AppCons a b) =
	convert (ExpOp_ Div a b)
	convert (Tuple a b) = -}

	-- TODO: only looks up local function, expand?
	convert (AST.Id name) = yield $ Ex $ MEM (TEMP name)
	--convert (AST.Id name) =  getVarLoc name >-> \k -> Ex $ MEM (BINOP PLUS (TEMP "fp") (CONST k))

instance Convert AST.Stmt where
	convert (AST.If cond stmt) = convert cond !++! unCx # convert stmt !-+! unNx # newLabel # newLabel >-> \(((c,s),t),f) -> Nx $ seq [c t f, LABEL t, s ,LABEL f]
--	convert (AST.IfElse cond stmt1 stmt2) = \((o, x, y) -> CJUMP o x y (convert stmt1) (convert stmt2)) (getCond cond)
	convert (AST.While cond stmt) = convert cond # convert stmt !++! uncurry parseWhile
		where
		parseWhile c s = newLabel # newLabel !++! \(b,d) -> (unCx c >-> \x -> (x b d, b, d)) # unNx s # newLabel  >-> \(((cc, b, d),ss),t) -> Nx (seq [LABEL t, cc, LABEL b, ss, JUMP (NAME t) [], LABEL d])
	convert (AST.Assign id exp) = convert exp !++! unEx >-> \e -> Nx $ MOVE (MEM (TEMP id)) e
	--convert (AST.Assign id exp) = \e -> Nx (MOVE (MEM (BINOP PLUS (TEMP "fp") (CONST k))) (unEx $ convert exp e))
	convert (AST.Seq stmt) = (iter (\x -> convert x !++! unNx) stmt) >-> \x -> Nx (seq x)
{-	convert (AST.FunCall_ funCall) =
	convert (AST.Return a) = -}

instance Convert AST.VarDecl where
	convert (AST.VD t id exp) = convert exp !++! unEx >-> \e -> Nx $ MOVE (MEM (TEMP id)) e
--	convert (AST.VD t id exp) = addVarToFrame id # convert exp !++! \(k,e) -> unEx e >-> (\x -> (k,x)) >-> \(k,e) -> Nx $ MOVE (MEM (BINOP PLUS (TEMP "fp") (CONST k))) e



