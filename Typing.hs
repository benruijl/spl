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

-- FIXME: Only allowed to be called with Generic_ and concrete types, so no List (Generic "a")
addMap2 :: Type -> Type -> TypeChecker
addMap2 a b = \e@(i, m, n) ->  case a of
	Generic_ _ -> case b of 
	--	Generic_ _ -> if a == b then yield a e else if isIncluded a b then addMap b a e else addMap a b e -- is this correct?
		c -> if a /=b && isIncluded a b then error $ "Cannot unify types: " ++ show a ++ "," ++ show b else addMap a b e
	d ->  case b of 
	--	Generic_ _ -> addMap b d e
		c -> if a /=b && isIncluded a b then error $ "Cannot unify types: " ++ show a ++ "," ++ show b else addMap b a e

addMap3 :: Type -> Type -> Env -> Env
addMap3 (List_ a) (List_ b) = addMap3 a b
addMap3 (Tuple_ a b) (Tuple_ d e) = addMap3 a b . addMap3 d e
--TODO: also do the other way around?
addMap3 (Generic_ a) b = if isIncluded (Generic_ a) b then error $ "Cannot unify types: " ++ show a ++ "," ++ show b else addMap4 (Generic_ a) b
addMap3 a (Generic_ b) = if isIncluded a (Generic_ b) then error $ "Cannot unify types: " ++ show a ++ "," ++ show b else addMap4 (Generic_ b) a 
addMap3 a b =  if a == b then id else error $ "Cannot unify types: " ++ show a ++ "," ++ show b

-- Add map to environment and check if it is compatible
addMap4 :: Type -> Type -> Env -> Env
addMap4 a@(Generic_ _) b = \e@(i, m, n) -> case find ((==a).fst) m of
	Just (_, c) ->  addMap3 b c e -- Add a map from b to c
	Nothing -> (i, (a, b) : m, n)
addMap4 a b = if a == b then id else error $ "Cannot unify types: " ++ show a ++ "," ++ show b

-- Add map to environment and check if it is compatible
addMap :: Type -> Type -> TypeChecker
addMap a@(Generic_ _) b = \e@(i, m, n) -> case find ((==a).fst) m of -- this means if it is already in the environment
	Just (_, c) ->  addMap2 b c e -- Add a map from b to c
	Nothing -> yield b (i, (a, b) : m, n)
addMap a b = if a == b then yield a else error $ "Cannot unify types: " ++ show a ++ "," ++ show b

-- check if the type a is in the right part
-- useful, because such substitutions are not allowed

isIncluded :: Type -> Type -> Bool
isIncluded k (Tuple_ a b) = isIncluded k a || isIncluded k b
isIncluded k (List_ a) = isIncluded k a
isIncluded a b = a == b

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
	getType :: a -> Type
	
instance TypeCheck VarDecl where
	enforce (VD t name exp) = \e -> addMap2 t (fst $ enforce exp e) e -- FIXME: new e should be passed to addMap

instance TypeCheck FunDecl where
	
	getType (FD ret name args vars stmts) = Function (map fst args) ret
	
instance TypeCheck Exp where	
		
	enforce (Int _) = yield Int_
	enforce (Bool _) = yield Bool_
	enforce (ExpOp_ o a b) = \e-> if fst ((enforce a)e) == fst ((enforce b)e) && fst ((enforce a)e) == Int_ then (yield Int_) e else error $ "Expected type int " ++ "," ++ " int but got " ++ show (fst ((enforce a)e)) ++ "," ++ show (fst ((enforce b)e))
	enforce (ExpOp_ o a b) = \e-> if fst ((enforce a)e) == fst ((enforce b)e) && fst ((enforce a)e) == Bool_ then (yield Bool_) e else error $ "Expected type bool " ++ "," ++ " int but got " ++ show (fst ((enforce a)e)) ++ "," ++ show (fst ((enforce b)e))
	enforce (Op1_ o a) = \e-> if fst ((enforce a)e) == Int_ then (yield Int_) e else error $ "Expected int, but got " ++ show (fst ((enforce a)e))
	enforce (Op1_ o a) = \e-> if fst ((enforce a)e) == Bool_ then (yield Bool_) e else error $ "Expected bool, but got " ++ show (fst ((enforce a)e))
	enforce EmptyList = freshVar >-> (\x -> List_ x)
	enforce (Tuple a b) = enforce a +=+ enforce b >-> (\(c, d) -> Tuple_ c d)
	enforce (Id name) = \e@(i,m,n) -> case find (\(i,t) -> i == name) n of 
											Just (i, t) -> yield t e
											Nothing -> error $ "Undefined variable '" ++ name ++ "'"		
	enforce (FunCall (name, args)) = \e@(i,m,n) -> case find (\(i,t) -> i == name) n of
											-- TODO: check if number of arguments is the same
											-- FIXME: too complicated, need for more combinators
											Just (i, Function v r) -> (\(x,y) -> (r, y)) $ (\s -> iter (\(a,b) -> addMap2 a b) (zip (fst s) v) (snd s)) $ iter enforce args e
											_ -> error $ "Undefined function '" ++ name ++ "'"

instance TypeCheck Stmt where	
--  TO DO how to treat compound types such as lists and tuples

	enforce (Assign ids exp) = \e@(i,m,n) -> if fst ((enforce (Id ids))e) == fst ((enforce exp)e) then (yield (fst ((enforce exp)e))) e else error $ "Cannot unify types: " ++ show (fst ((enforce (Id ids))e)) ++ "," ++ show (fst ((enforce exp)e))
	enforce (If exp stmt) =  \e-> if ((fst ((enforce exp)e)) == Bool_) then (yield Undefined)e else error("Cannot unify expected type Bool_ with " ++ show((enforce exp)e) ++"!")
	enforce (IfElse exp stmt1 stmt2) = \e->if ((fst ((enforce exp) e)) == Bool_) then (yield Undefined)e else error("Cannot unify expected type Bool_ with " ++ show((enforce exp) e) ++"!")
	enforce (While exp stmt) = \e->if ((fst ((enforce exp)e)) == Bool_) then (yield Undefined)e else error("Cannot unify expected type Bool_ with " ++ show((enforce exp)e) ++"!")
	enforce (Seq stmt) = yield Undefined
	enforce (FunCall_ funCall) = yield Undefined
	enforce (Return exp) = enforce exp -- TODO: check if it matches with the function specification (the majority are undefined except assign
									   -- some way to pass the id of the current function that is executed as well as variables
