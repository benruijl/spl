import AST
import Data.List

-- Environment should keep track of the current Prog and all the function names,
-- it should also keep track of the function the checker is currently in.
-- Env = (fresh var count, typemap, symbol table)
type Env = (Int, [(Type, Type)],[(Id, Type)]) 
type TypeChecker =  Env -> (Type, Env)

-- Generate a new type

freshVar :: TypeChecker
freshVar = \(i,m,n)->(Generic_ ("_a" ++ show (i + 1)), (i + 1, m, n))

-- check if the type a is included in type b
isIncluded :: Type -> Type -> Bool
isIncluded k (Tuple_ a b) = isIncluded k a || isIncluded k b
isIncluded k (List_ a) = isIncluded k a
isIncluded a b = a == b

unify :: Type -> Type -> Env -> Env
unify (List_ a) (List_ b) = unify a b
unify (Tuple_ a b) (Tuple_ d e) = unify a d . unify b e
unify (Generic_ a) b = if isIncluded (Generic_ a) b then error $ "Cannot unify types: " ++ a ++ "," ++ show b else addMap (Generic_ a) b
unify a (Generic_ b) = unify (Generic_ b) a
unify a b =  if a == b then id else error $ "Cannot unify types: " ++ show a ++ " and " ++ show b

-- Gets the type of a variable or function
get :: Id -> Env -> (Type, Env)
get id = \e@(i,m,n) -> case find (\(i,t) -> i == id) n of 
	Just (i, t) -> (t, e)
	Nothing -> error $ "Undefined variable or function '" ++ id ++ "'"

-- Add variable or function to the symbol table
set :: Id -> Type -> Env -> Env
set id t = \e@(i,m,n) -> (i, m, (id, t) : n)

-- Adds a map from a generic to another type
addMap :: Type -> Type -> Env -> Env
addMap a@(Generic_ _) b = \e@(i, m, n) -> case find ((==a).fst) m of
	Just (_, c) ->  unify b c e -- Add a map from b to c
	Nothing -> (i, (a, b) : m, n)
addMap a b = if a == b then id else error $ "Cannot unify types: " ++ show a ++ " and " ++ show b


infixl 5 >->
(>->) :: (Env -> (a, Env)) -> (a -> b) -> (Env -> (b, Env))
(>->) p k cs =
  case p cs of
    (a, cs') -> (k a, cs')
	
infixl 6 +=+
(+=+) :: TypeChecker -> TypeChecker -> Env -> ((Type, Type), Env)
(+=+) a b e = case a e of
	(t1, e1) -> 
		case b e1 of
			(t2, e2) -> ((t1,t2),e2)
			
-- infixl 1 >>-
-- (>>-) :: TypeChecker -> (Type -> TypeChecker) -> TypeChecker
-- x >>- y = \env -> \(t,env) -> ((y t) env) (x env)

-- infixl 1 >>|
-- (>>|) :: TypeChecker -> TypeChecker -> TypeChecker 
-- x >>| y = x >>- \_-> y

iter :: (a -> TypeChecker) -> [a] -> Env -> ([Type], Env)
iter f [] e = ([], e)
iter f l e = ([fst h] ++ fst t, snd t)
	where
		h = f (head l) e 
		t = iter f (tail l) (snd h)

			
yield :: Type -> TypeChecker
yield x = \e -> (x,e)

class Substitute a where
	substitute :: (Type, Type) -> a -> a
	typeScraper :: a -> [Type]
	makeUnique :: a -> [(Type,Type)] -> a
	uniqueName :: a -> TypeChecker
	substInEnv :: a -> TypeChecker
	
instance Substitute Decl where
	substitute (a,b) (VarDecl v)  = VarDecl (substitute (a,b) v)
	substitute (a,b) (FunDecl f) =  FunDecl (substitute (a,b) f)

-- BUG: first occurence is not replaced

instance Substitute FunDecl where
	substitute (a,b) (FD ret name args vars stmts) = FD (substitute (a,b) ret) name (newargs args) (map (substitute (a,b)) vars) stmts
		where
		newargs = map (\(x,i) -> if (x == a) then (b,i) else (x, i))
	typeScraper (FD ret name args vars stmts) = typeScraper ret ++ (concatMap (typeScraper . fst) args) ++ (concatMap typeScraper vars)
	uniqueName f = \(i,m,n) -> (Undefined,(length vars + i,(zip vars [Generic_ ("_a" ++ show (x + 1)) | x <- [i..]]) ++ m, n))
		where
		vars = nub $ typeScraper f

--	substInEnv f = \(i,m,n) -> \k@(FD ret name args vars stmts)->(Undefined,(i,m,[(getType k,name)])) (makeUnique f m)
		
	makeUnique f [(a, b)] =  substitute (a, b) f
	makeUnique f l = makeUnique f (tail l)
   
instance Substitute VarDecl where
	substitute (a,b) (VD t name e) = VD (substitute (a,b) t) name e
	typeScraper (VD t name e) = typeScraper t
	uniqueName f  = \(i,m,n) -> (Undefined,(i + 1,(zip (typeScraper f) [(Generic_ ("_a" ++ show (i + 1)))]) ++ m, n))
	
--  substInEnv v = \(i,m,n) -> \k@(VD t name e)->(Undefined,(i,m,[(t,name)])) (makeUnique v m)
	
	makeUnique v [(a, b)] =  substitute (a, b) v
	makeUnique v l = makeUnique v (tail l)

instance Substitute Type where
	substitute (a,b) c = if (a == c) then b else c -- here I add the rulles
	typeScraper k@(Generic_ t) = [k]
	typeScraper _ = []

fullSubstitute :: (Type,Type) -> Prog -> Prog
fullSubstitute (a,b) c = map (substitute (a,b)) c

class TypeCheck a where

	-- what should this function return? If it only modifies the environment, it is maybe ok as well.
	
	enforce :: a -> Env -> (Type, Env)
	enforce2 :: a -> Env -> Env
	getType :: a -> Env -> Type
	
instance TypeCheck VarDecl where
	enforce2 (VD t name exp) = \e -> (set name t . unify t (getType exp e)) e

instance TypeCheck FunDecl where
	
	getType (FD ret name args vars stmts) = \_ -> Function (map fst args) ret
	
instance TypeCheck Exp where
	getType (Int _) = const Int_
	getType (Bool _) = const Bool_
	getType (Tuple a b) = \e -> Tuple_ (getType a e) (getType b e)
		
	enforce (Int _) = yield Int_
	enforce (Bool _) = yield Bool_
	enforce (ExpOp_ o a b) = \e-> if fst ((enforce a)e) == fst ((enforce b)e) && fst ((enforce a)e) == Int_ then (yield Int_) e else error $ "Expected type int " ++ "," ++ " int but got " ++ show (fst ((enforce a)e)) ++ "," ++ show (fst ((enforce b)e))
	enforce (Op1_ o a) = \e-> if fst ((enforce a)e) == Int_ then (yield Int_) e else error $ "Expected int, but got " ++ show (fst ((enforce a)e))
	enforce EmptyList = freshVar >-> (\x -> List_ x)
	enforce (Tuple a b) = enforce a +=+ enforce b >-> (\(c, d) -> Tuple_ c d)
	enforce (Id name) = get name		
--	enforce (FunCall (name, args)) = \e@(i,m,n) -> case find (\(i,t) -> i == name) n of
											-- TODO: check if number of arguments is the same
											-- FIXME: too complicated, need for more combinators
			--								Just (i, Function v r) -> (\(x,y) -> (r, y)) $ (\s -> iter (\(a,b) -> addMap2 a b) (zip (fst s) v) (snd s)) $ iter enforce args e
--											_ -> error $ "Undefined function '" ++ name ++ "'"

instance TypeCheck Stmt where	
	enforce2 (If cond stmt) = (\e -> unify (getType cond e) Bool_ e) . enforce2 stmt
	enforce2 (IfElse cond stmt1 stmt2) = (\e -> unify (getType cond e) Bool_ e) . enforce2 stmt1 . enforce2 stmt2
	enforce2 (Assign id exp) = \e -> unify (fst (get id e)) (getType exp e) e
	enforce2 (While cond stmt) = (\e -> unify (getType cond e) Bool_ e) . enforce2 stmt
	enforce2 (Seq stmt) = \e -> foldl (\x y -> enforce2 y x) e stmt
--	enforce2 (FunCall_ funCall) =
	enforce2 (Return exp) = enforce2 exp -- TODO: check if it matches with the function specification (the majority are undefined except assign
									   -- some way to pass the id of the current function that is executed as well as variables
