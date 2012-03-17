import AST

type EnvVar = Id -> Val
type EnvFun = Id -> FunDecl
type Env = ([EnvVar],EnvFun,Bool) -- we need Bool in order to be aware of context for example if(cond) return then statements executed afterwards should not modify the environment
type Sem = Env -> (Val, Env)

data Val = Int__ Int | Bool__ Bool | Tuple__ Val Val | List__ [Val] | Unknown Int | Undefined deriving (Show) -- like AbstractTypeSort

pop :: [a] -> [a]
pop (hdL:[]) = [hdL]
pop (hdL:tlL) = tlL

emptyVal = \var -> Undefined
emptyFun = \fun -> Undefined


class Semantics a  where 
	sem :: a -> Sem

rtn :: Val -> Sem
rtn n = \(ev,ef,flg) -> (n, ((pop ev),ef,flg))

infixl 1 >>-
(>>-) :: Sem -> (Val -> Sem) -> Sem
x >>- y = \(ev,ef,flg) -> (\(i,(ev,ef,flg)) -> if (flg) then (i,(ev,ef,True)) else ((y i) (ev,ef,flg))) (x (ev,ef,flg))

infixl 1 >>|
(>>|) :: Sem -> Sem -> Sem 
x >>| y = x >>- \_-> y

get :: Id -> Sem
get var = (readLoc var) >>- next
		where	
			readLoc :: Id -> Sem
			readLoc var = \(ev,ef,flg) -> ((head ev var), (ev,ef,flg))
			next Undefined  = readGlob var
				where	
					readGlob :: Id -> Sem
					readGlob var = \(ev,ef,flg) -> ((last ev var), (ev,ef,flg))
			next x	= rtn x
{-
infixl 1 |->
(|->) :: Id Val -> Sem
var |-> val = \(ev,ef,flg) -> check ((hd ev) var) (ev,ef,flg)
	    	  where
	    	  		check :: Val -> Sem
	  				check UndefVal = writeGlob var val 
							where
								writeGlob :: Id Val -> Sem
								writeGlob v i = \(ev,ef,flg) -> (i, (reverse ([\v'-> if (v' == v) i (last (ev v'))]++(tail(reverse ev))),ef,flg))
	  				check _ = writeLoc var val
							where
								writeLoc :: Id Val -> Sem
								writeLoc v i = \(ev,ef,flg) -> (i, ([\v'-> if (v' == v) i (head (ev v'))]++(tail ev),ef,flg))
							
mapSem :: [VarDecl] -> Sem
mapSem (hdL:[]) = sem hdL
mapSem (hdL:tlL) = sem hdL >>| mapSem tlL


instance Semantics VarDecl
	where
		sem var = (sem (snd var)) >>- \expVal -> (snd(fst var)) |-> expVal
	

instance Semantics FunDecl
	where
		sem(FD _ _ varDecls stmts) =   (mapSem varDecls) >>| (sem stmts)

-}

instance Semantics Exp
	where
		sem (Int n)	     = rtn (Int__ n)
		sem (Id v)		 = get v
		sem (ExpOp_ op2 e1 e2) = 	sem e1 
							>>-	\l -> sem e2
							>>-  \r -> rtn ((toFun2 op2) l r)
    
-- sem (Call (idFun,actArgs)) = funCall idFun actArgs

instance Semantics Stmt
		where
{--
			sem (Assign v e)	= 	sem e
										>>- \i -> v |-> i
--}

			sem (If c tt)       = 	sem c
												>>- \(Bool__ i) -> if (i == True) then (sem tt) else (\e->(Undefined,e))


			sem (IfElse c tt ff)	= 	sem c
							>>- \(Bool__ i) -> if (i == True) then (sem tt) else (sem ff)

			sem (While c bdy)	= 	sem c >>- \(Bool__ i) -> if (i == True) then
															(	
																sem bdy >>| sem (While c bdy)
															) 
															rtn (Bool__ i)
															else rtn (Bool__ i)
	
			sem (Seq stm1 stm2)	=	sem stm1 
								>>| sem stm2
	
			sem (List__ [stmt:stmts]) = sem stmt >>| sem (List__ stmts)
	
			sem (Return exp)  = sem exp
	

			sem (FunCall exp) = sem exp
	
opIntIntToVal :: (Int -> Int -> Int) -> Val -> Val -> Val
opIntIntToVal op (Int__ x) (Int__ y) = Int__ (op x y)

opBoolBoolToVal :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
opBoolBoolToVal op (Bool__ x) (Bool__ y) = Bool__ (op x y)

opIntBoolToVal :: (Int -> Int -> Bool) -> Val-> Val -> Val
opIntBoolToVal op (Int__ x) (Int__ y) = Bool__ (op x y)

opIntToVal ::(Int -> Int) -> Val -> Val
opIntToVal op (Int__ x) = Int__ (op x)

opBoolToVal ::(Bool -> Bool) -> Val -> Val
opBoolToVal op (Bool__ x) = Bool__ (op x)

instance Eq Val
	where
		(==)(Int__ x) (Int__ y) = x==y
		(==)(Bool__ x) (Bool__ y) = x==y
		(==)(List__ x) (List__ y) = (head x == head y) && (tail x == tail y)
		(==)(Tuple__ x1 y1) (Tuple__ x2 y2) =  (x1 == x2) && (y1 == y2)

toFun2 :: ExpOp -> (Val -> Val -> Val)
toFun2 Add = opIntIntToVal (+)
toFun2 Sub = opIntIntToVal (-) 
toFun2 Mul = opIntIntToVal (*)
toFun2 Mod = opIntIntToVal (rem)
toFun2 Div = opIntIntToVal (div)
toFun2 NotEq = opIntBoolToVal (/=)
toFun2 Less	= opIntBoolToVal (<)
toFun2 More = opIntBoolToVal (>)
toFun2 LessEq = opIntBoolToVal (<=)
toFun2 MoreEq = opIntBoolToVal (>=)
toFun2 Equals =  opIntBoolToVal (==)
toFun2 And = opBoolBoolToVal (&&)
toFun2 Or = opBoolBoolToVal (||)

toFun1 :: Op1 -> Val -> Val
toFun1 Negate = opBoolToVal (not) 
toFun1 UnitaryMinus = opIntToVal (\x -> (-1) * x)