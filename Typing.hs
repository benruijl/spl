import AST

data AbstractType = Int__ | Bool__ | Tuple__ AbstractType AbstractType | List__ AbstractType | Unknown Int | Undefined deriving (Eq, Show)

-- sketch of the environment
type Env = (([(Id, Int)],[(Id, FunDecl)]),[(Id, VarDecl)])

emptyEnv = (([],[]),[])

-- TODO: get type of Id from the environment, fix EmptyList and add support for booleans
getType :: Env -> Exp -> AbstractType
getType e (Int _) = Int__
getType e (Bool _) = Bool__
getType e (ExpOp_ o a b) = if getType e a == getType e b && getType e a == Int__ then Int__ else error $ "Expected type int " ++ "," ++ " int but got " ++ show (getType e a) ++ "," ++ show (getType e b)
getType e (Op1_ o a) = if (getType e a == Int__) then Int__ else error $ "Expected int, but got " ++ show (getType e a)
getType e EmptyList = Unknown 1 -- dummy id
getType e (Tuple a b) = Tuple__ (getType e a) (getType e b)
--getType e (Id_ v) = envGetType e v
--getType e (FunCall (f, _)) = getReturnType e f
