import AST

data AbstractType = Int__ | Bool__ | Tuple__ AbstractType AbstractType | List__ AbstractType | Unknown Int | Undefined deriving (Eq, Show)

-- sketch of the environment
type Env = (([(Id, Int)],[(Id, FunDecl)]),[(Id, VarDecl)])

emptyEnv = (([],[]),[])

-- TODO: get type of Id from the environment, fix EmptyList and add support for booleans
getType1 :: Env -> Exp -> AbstractType
getType1 e (Int _) = Int__
getType1 e (Bool _) = Bool__
getType1 e (ExpOp_ o a b) = if getType1 e a == getType1 e b && getType1 e a == Int__ then Int__ else error $ "Expected type int " ++ "," ++ " int but got " ++ show (getType1 e a) ++ "," ++ show (getType1 e b)
getType1 e (Op1_ o a) = if (getType1 e a == Int__) then Int__ else error $ "Expected int, but got " ++ show (getType1 e a)
getType1 e EmptyList = Unknown 1 -- dummy id
getType1 e (Tuple a b) = Tuple__ (getType1 e a) (getType1 e b)
--getType1 e (Id_ v) = envGetType e v
--getType1 e (FunCall (f, _)) = getReturnType e f

getType2 :: Env -> Stmt -> AbstractType
--getType2 e (Assign id1 exp) = if (getType1 e exp) == (getType1 e id1) then (getType1 e id1)
					--	  else error("Cannot unify types !" ++ show (getType1 e exp) ++ show(getType1 e id1)) so what should we do with id stuff?
getType2 e (If exp stmt) = if (getType1 e exp) == Bool__ then Undefined
						  else error("Cannot unify expected type Bool_ with " ++ show(getType1 e exp) ++"!");
getType2 e (IfElse exp stmt1 stmt2) = if (getType1 e exp) == Bool__ then Undefined
						  else error("Cannot unify expected type Bool_ with " ++ show(getType1 e exp) ++"!");
getType2 e (While exp stmt) = if (getType1 e exp) == Bool__ then Undefined
						  else error("Cannot unify expected type Bool_ with " ++ show(getType1 e exp) ++"!");
getType2 e (Seq stmt) = Undefined
getType2 e (FunCall_ funCall) = Undefined
getType2 e (Return exp) = getType1 e exp