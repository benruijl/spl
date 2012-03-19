import AST

-- All variables are typed in SPL, except [], so add a type for that
data AbstractType = Generic Id | Int__ | Bool__ | Tuple__ AbstractType AbstractType | List__ AbstractType | Undefined deriving (Eq, Show)
type Env = Int -- count of generic variables

type TypeChecker =  Env -> (AbstractType, Env)

-- Generate a new type
freshVar :: TypeChecker
freshVar x = (Generic ("a" ++ show (x + 1)), x + 1)

infixl 6 +=+
(+=+) :: TypeChecker -> TypeChecker -> Env -> ((AbstractType, AbstractType), Env)
(+=+) a b e = case a e of
	(t1, e1) -> 
		case b e1 of
			(t2, e2) -> ((t1,t2),e2)

infixl 5 >->
(>->) :: (Env -> (a, Env)) -> (a -> b) -> (Env -> (b, Env))
(>->) p k cs =
  case p cs of
    (a, cs') -> (k a, cs')

yield :: a -> (Env -> (a, Env))
yield x = \e -> (x,e) 
		
-- TODO: add support for booleans
getType1 :: Exp -> TypeChecker
getType1 (Int _) = yield Int__
getType1 (Bool _) = yield Bool__
--getType1 (ExpOp_ o a b) = if fst (getType1 a) == fst (getType1 b) && fst (getType1 a) == Int__ then yield Int__ else error $ "Expected type int " ++ "," ++ " int but got " ++ show (fst $ getType1 a) ++ "," ++ show (fst $ getType1 b)
--getType1 (Op1_ o a) = if (fst (getType1 a == Int__) then yield Int__ else error $ "Expected int, but got " ++ show (fst $ getType1 a)
getType1 EmptyList = freshVar
getType1 (Tuple a b) = getType1 a +=+ getType1 b >-> (\(c, d) -> Tuple__ c d)
--getType1 (Id_ v) = envGetType e v
--getType1 (FunCall (f, _)) = getReturnType e f

getType2 :: Env -> Stmt -> AbstractType
--getType2 e (Assign id1 exp) = if (getType1 e exp) == (getType1 e id1) then (getType1 e id1)
					--	  else error("Cannot unify types !" ++ show (getType1 e exp) ++ show(getType1 e id1)) so what should we do with id stuff?
getType2 e (If exp stmt) = if (fst $ getType1 exp e) == Bool__ then Undefined
						  else error("Cannot unify expected type Bool_ with " ++ show(getType1 exp e) ++"!");
getType2 e (IfElse exp stmt1 stmt2) = if (fst $ getType1 exp e) == Bool__ then Undefined
						  else error("Cannot unify expected type Bool_ with " ++ show(getType1 exp e) ++"!");
getType2 e (While exp stmt) = if (fst $ getType1 exp e) == Bool__ then Undefined
						  else error("Cannot unify expected type Bool_ with " ++ show(getType1 exp e) ++"!");
getType2 e (Seq stmt) = Undefined
getType2 e (FunCall_ funCall) = Undefined
getType2 e (Return exp) = fst $ getType1 exp e
