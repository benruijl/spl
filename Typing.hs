import AST
import Data.List

-- Environment should keep track of the current Prog and all the function names,
-- it should also keep track of the function the checker is currently in.
type Env = (Int, [(Type, Type)]) 
type TypeChecker =  Env -> (Type, Env)

-- Generate a new type
freshVar :: TypeChecker
freshVar (x, m) = (Generic_ ("_a" ++ show (x + 1)), (x + 1, m))

-- Add map to environment and check if it is compatible
addMap :: Type -> Type -> Env -> Env
addMap a@(Generic_ _) b e@(i, m) = case find ((==a).fst) m of
	Just (_, c@(Generic_ _)) -> addMap b c e -- Add a map from b to c
	Just (_, c) -> if (c == b) then e else error $ "Cannot unify types: " ++ show b ++ "," ++ show c
	Nothing -> (i, (a, b) : m)
addMap _ _ e = e

-- check if the type a is in the right part
-- useful, because such substitutions are not allowed
isIncluded :: Type -> Type -> Bool
isIncluded k (Tuple_ a b) = isIncluded k a || isIncluded k b
isIncluded k (List_ a) = isIncluded k a
isIncluded a b = a == b

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

instance Substitute Decl where
	substitute a b (VarDecl v) = VarDecl (substitute a b v)
	substitute a b (FunDecl f) = FunDecl (substitute a b f)

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


fullSubstitute :: Type -> Type -> Prog -> Prog
fullSubstitute a b c = map (substitute a b) c


class TypeCheck a where
	-- what should this function return? If it only modifies the environment, it is maybe ok as well.
	enforce :: a -> Env -> (Type, Env)

instance TypeCheck Exp where	
	-- TODO: add support for booleans
	enforce (Int _) = yield Int_
	enforce (Bool _) = yield Bool_
	--enforce (ExpOp_ o a b) = if fst (enforce a) == fst (enforce b) && fst (enforce a) == Int__ then yield Int__ else error $ "Expected type int " ++ "," ++ " int but got " ++ show (fst $ enforce a) ++ "," ++ show (fst $ enforce b)
	--enforce (Op1_ o a) = if (fst (enforce a == Int__) then yield Int__ else error $ "Expected int, but got " ++ show (fst $ enforce a)
	enforce EmptyList = freshVar >-> (\x -> List_ x)
	enforce (Tuple a b) = enforce a +=+ enforce b >-> (\(c, d) -> Tuple_ c d)
	--enforce (Id_ v) = envGetType e v
	--enforce (FunCall (f, _)) = getReturnType e f

instance TypeCheck Stmt where	
	--enforce e (Assign id1 exp) = if (enforce e exp) == (enforce e id1) then (enforce e id1)
						--	  else error("Cannot unify types !" ++ show (enforce e exp) ++ show(enforce e id1)) so what should we do with id stuff?
	--enforce (If exp stmt) = if (fst $ enforce exp e) == Bool_ then Undefined
	--						  else error("Cannot unify expected type Bool_ with " ++ show(enforce exp e) ++"!");
	--enforce (IfElse exp stmt1 stmt2) = if (fst $ enforce exp e) == Bool_ then Undefined
	--						  else error("Cannot unify expected type Bool_ with " ++ show(enforce exp e) ++"!");
	--enforce (While exp stmt) = if (fst $ enforce exp e) == Bool_ then Undefined
	--						  else error("Cannot unify expected type Bool_ with " ++ show(enforce exp e) ++"!");
	enforce (Seq stmt) = yield Undefined
	enforce (FunCall_ funCall) = yield Undefined
	enforce (Return exp) = enforce exp -- TODO: check if it matches with the function specification
