import AST
import Data.List

type Env = (Int, [(Type, Type)]) -- count of generic variables
type TypeChecker =  Env -> (Type, Env)

-- Generate a new type
freshVar :: TypeChecker
freshVar (x, m) = (Generic_ ("_a" ++ show (x + 1)), (x + 1, m))

addMap :: Type -> TypeChecker
addMap x (i, m) = (newVar, (i + 1, m ++ [(x, newVar)]))
	where
	newVar = Generic_ ("_a" ++ show (i + 1))

infixl 6 +=+
(+=+) :: TypeChecker -> TypeChecker -> Env -> ((Type, Type), Env)
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

class Substitute a where
	substitute :: Type -> Type -> a -> a
	typeScraper :: a -> [Type]
	uniqueName :: a -> Env -> ([(Type, Type)], Env)
	makeUnique :: a -> [(Type, Type)] -> a -- TODO: add env

-- BUG: first occurence is not replaced
instance Substitute FunDecl where
	substitute a b (FD ret name args vars stmts) = FD (substitute a b ret) name (newargs args) (map (substitute a b) vars) stmts
		where
		newargs = map (\(x,i) -> if (x == a) then (b,i) else (x, i))
	typeScraper (FD ret name args vars stmts) = typeScraper ret ++ (concatMap (typeScraper . fst) args) ++ (concatMap typeScraper vars)

	uniqueName f (i, e) = (zip vars [Generic_ ("_a" ++ show (x + 1)) | x <- [i..]], (length vars + i, e))
		where
		vars = nub $ typeScraper f

	makeUnique f [(a, b)] =  substitute a b f
	makeUnique f l = makeUnique f (tail l)

instance Substitute VarDecl where
	substitute a b (VD t name e) = VD (substitute a b t) name e
	typeScraper (VD t name e) = typeScraper t
	uniqueName f (i, e) = (zip (typeScraper f) [(Generic_ ("_a" ++ show (i + 1)))], (i + 1, e))

instance Substitute Type where
	substitute a b c = if (a == c) then b else c
	typeScraper k@(Generic_ t) = [k]
	typeScraper _ = []



		
-- TODO: add support for booleans
getType1 :: Exp -> TypeChecker
getType1 (Int _) = yield Int_
getType1 (Bool _) = yield Bool_
--getType1 (ExpOp_ o a b) = if fst (getType1 a) == fst (getType1 b) && fst (getType1 a) == Int__ then yield Int__ else error $ "Expected type int " ++ "," ++ " int but got " ++ show (fst $ getType1 a) ++ "," ++ show (fst $ getType1 b)
--getType1 (Op1_ o a) = if (fst (getType1 a == Int__) then yield Int__ else error $ "Expected int, but got " ++ show (fst $ getType1 a)
getType1 EmptyList = freshVar >-> (\x -> List_ x)
getType1 (Tuple a b) = getType1 a +=+ getType1 b >-> (\(c, d) -> Tuple_ c d)
--getType1 (Id_ v) = envGetType e v
--getType1 (FunCall (f, _)) = getReturnType e f

getType2 :: Env -> Stmt -> Type
--getType2 e (Assign id1 exp) = if (getType1 e exp) == (getType1 e id1) then (getType1 e id1)
					--	  else error("Cannot unify types !" ++ show (getType1 e exp) ++ show(getType1 e id1)) so what should we do with id stuff?
getType2 e (If exp stmt) = if (fst $ getType1 exp e) == Bool_ then Undefined
						  else error("Cannot unify expected type Bool_ with " ++ show(getType1 exp e) ++"!");
getType2 e (IfElse exp stmt1 stmt2) = if (fst $ getType1 exp e) == Bool_ then Undefined
						  else error("Cannot unify expected type Bool_ with " ++ show(getType1 exp e) ++"!");
getType2 e (While exp stmt) = if (fst $ getType1 exp e) == Bool_ then Undefined
						  else error("Cannot unify expected type Bool_ with " ++ show(getType1 exp e) ++"!");
getType2 e (Seq stmt) = Undefined
getType2 e (FunCall_ funCall) = Undefined
getType2 e (Return exp) = fst $ getType1 exp e
