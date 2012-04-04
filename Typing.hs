module Typing where

import AST
import Data.List

-- Environment should keep track of the current Prog and all the function names,
-- it should also keep track of the function the checker is currently in.
-- Env = (fresh var count, typemap, symbol table, current function id)
data Scope = Global | Local Id | FunctionCall deriving Show
type SymbolTable = ([(Type, Type)], [(Id, Type)])
type Env = (Int, SymbolTable, SymbolTable, SymbolTable, Scope) 
type TypeChecker =  Env -> (Type, Env)

showEnv :: Env -> String
showEnv (i,m,l,f,c) = "Fresh var count: " ++ show i ++ "\nGlobal symbol table:" ++ show m ++ "\nLocal symbol table: " ++ show l ++ "\nFunction call symbol table: " ++ show f ++ "\nCurrent scope: " ++ show c

-- Generate a new type

freshVar :: TypeChecker
freshVar = \(i,m,l,f,c)->(Generic_ ("_a" ++ show (i + 1)), (i + 1, m, l,f,c))

-- check if the type a is included in type b
isIncluded :: Type -> Type -> Bool
isIncluded k (Tuple_ a b) = isIncluded k a || isIncluded k b
isIncluded k (List_ a) = isIncluded k a
isIncluded a b = a == b

unify :: Type -> Type -> Env -> Env
unify (List_ a) (List_ b) = unify a b
unify (Tuple_ a b) (Tuple_ d e) = unify a d . unify b e
unify (Generic_ a) b = if isIncluded (Generic_ a) b then error $ "Cannot unify types: " ++ a ++ " and " ++ show b else addMap (Generic_ a) b
unify a (Generic_ b) = unify (Generic_ b) a
unify a b =  if a == b then id else error $ "Cannot unify types: " ++ show a ++ " and " ++ show b

-- Gets the function type the checker is currently in
getLocalFunc :: Env -> (Type, Env)
getLocalFunc e@(i,m,n,l,Local id) = getSymbol id e
getLocalFunc _ =  error $ "Not in function"

-- Sets the current scope
setScope :: Scope -> Env -> Env
setScope s = \e@(i,m,l,f,c) -> (i, m, l, f, s)

-- Clear the local table
-- TODO: store in another map
clearLoc :: Env -> Env
clearLoc = \e@(i,m,l,f,c) -> (i,m,([],[]),f,c)

-- Add variable or function to the global symbol table
addGlob :: SymbolTable -> Env -> Env
addGlob (t, d) = \(i,(ot, od),l,f,c) -> (i,(t ++ ot, d ++ od),l,f,c)

addLoc :: SymbolTable -> Env -> Env
addLoc (t, d) = \(i,g,(ot, od),f,c) -> (i,g,(t ++ ot, d ++ od),f,c)

-- Adds symbols in the function scope
addFunc :: SymbolTable -> Env -> Env
addFunc (t, d) = \(i,g,l,(ot, od),c) -> (i,g,l,(t ++ ot, d ++ od),c)

addSymbol :: (Id, Type) -> Env -> Env
addSymbol s e@(i, m, l, f, Global) = addGlob ([], [s]) e
addSymbol s e@(i, m, l, f, Local id) = addLoc ([], [s]) e
addSymbol s e@(i, m, l, f, FunctionCall) = addFunc ([], [s]) e

addSymbolType :: (Type, Type) -> Env -> Env
addSymbolType s e@(i, m, l, f, Global) = addGlob ([s], []) e
addSymbolType s e@(i, m, l, f, Local id) = addLoc ([s], []) e
addSymbolType s e@(i, m, l, f, FunctionCall) = addFunc ([s], []) e

getSymbol :: Id -> Env -> (Type, Env)
getSymbol id e@(i, (_, s), l, f, Global) = case find (\(i,t) -> i == id) s of
	Just (i, t) -> (t, e)
	Nothing -> error $ "Undefined variable or function '" ++ id ++ "', " ++ showEnv e
getSymbol id e@(i, m, l@(_, s), f, _) = case find (\(i,t) -> i == id) s of
	Just (i, t) -> (t, e)
	Nothing -> fst (getSymbol id (i, m, l, f, Global), e) -- search globally

getSymbolType t e@(_, (s, _), _, _, c) = getSymbolType' t c e

getSymbolType' :: Type -> Scope -> Env-> Maybe (Type, Type) -- TODO: should it give Env back?
getSymbolType' id Global e@(_, (s, _), _, _, _) = find (\(i,t) -> i == id) s
getSymbolType' id (Local _) e@(_, _, (s, _), _, _) = case find (\(i,t) -> i == id) s of
	Just (i, t) -> Just (i, t)
	Nothing -> getSymbolType' id Global e
getSymbolType' id (Local _) e@(_, _, (s, _), _, _) = case find (\(i,t) -> i == id) s of
	Just (i, t) -> Just (i, t)
	Nothing -> getSymbolType' id (Local "") e -- TODO: does it matter what the function is?

-- Adds a map from a generic to another type
addMap :: Type -> Type -> Env -> Env
addMap a@(Generic_ _) b = \e -> case getSymbolType a e of
	Just (_, c) ->  unify b c e -- Add a map from b to c
	Nothing -> addSymbolType (a, b) e
addMap a b = if a == b then id else error $ "Cannot unify types: " ++ show a ++ " and " ++ show b

-- Builds a global environment from a program
buildEnv :: Prog -> Env
buildEnv p = foldl (\x y -> case y of 
		 (VarDecl (VD _ name _)) -> addSymbol (name, getType y x) x
		 (FunDecl (FD _ name _ _ _)) -> addSymbol (name, getType y x) x)
		(defaultFunctions (0, ([],[]), ([],[]), ([],[]), Global)) p

-- Adds the default functions
-- Assumes the Scope is set to Global
defaultFunctions :: Env -> Env
defaultFunctions = listDo addSymbol [hd, tl, isEmpty, fst, snd, print]
	where
	hd = ("head", Function [List_ (Generic_ "_r1")] (Generic_ "_r1"))
	tl = ("tail", Function [List_ (Generic_ "_r2")] (List_ (Generic_ "_r2")))
	isEmpty = ("isEmpty", Function [List_ (Generic_ "_r3")] Bool_)
	fst = ("fst", Function [Tuple_ (Generic_ "_r4") (Generic_ "_r5")] (Generic_ "_r4"))
	snd = ("snd", Function [Tuple_ (Generic_ "_r6") (Generic_ "_r7")] (Generic_ "_r7"))
	print = ("print", Function [Generic_ "_r8"] Void)
	

-- TODO: keep track of global and local environment
-- TODO: set the function that is about to get enforced
fullEnforce :: Prog -> Env -> Env
fullEnforce p e = foldl (\x y -> enforce y x) e p

listDo :: (a -> Env -> Env) -> [a] -> Env -> Env
listDo a l = \e -> foldl (\x y -> a y x) e l

listEnforce l = listDo enforce l


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
--	uniqueName f = \(i,m,n,f,c) -> (Undefined,(length vars + i,(zip vars [Generic_ ("_a" ++ show (x + 1)) | x <- [i..]]) ++ m, n,f,c))
--		where
--		vars = nub $ typeScraper f

--	substInEnv f = \(i,m,n) -> \k@(FD ret name args vars stmts)->(Undefined,(i,m,[(getType k,name)])) (makeUnique f m)
		
	makeUnique f [(a, b)] =  substitute (a, b) f
	makeUnique f l = makeUnique f (tail l)
   
instance Substitute VarDecl where
	substitute (a,b) (VD t name e) = VD (substitute (a,b) t) name e
	typeScraper (VD t name e) = typeScraper t
	--uniqueName f  = \(i,m,n,a) -> (Undefined,(i + 1,(zip (typeScraper f) [(Generic_ ("_a" ++ show (i + 1)))]) ++ m, n,a))
	
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
	enforce :: a -> Env -> Env
	-- Note: getType should not modify the state
	getType :: a -> Env -> Type

instance TypeCheck Decl where
	enforce (FunDecl f) = enforce f
	enforce (VarDecl v) = enforce v
	getType (FunDecl f) = getType f
	getType (VarDecl v) = getType v
	
instance TypeCheck VarDecl where
	-- VarDecl has its own scope
	enforce (VD t name exp) = \e -> (unify t (getType exp e) . addSymbol (name, t) . (setScope (Local name)) .clearLoc) e
	getType (VD t name exp) = const t

instance TypeCheck FunDecl where
	-- TODO: all functions and all global variables are added to the environment twice now
	-- FIXME: all args are added to the global state
	-- TODO: now the current function is written here, do somewhere else?
	enforce (FD ret name args vars stmts) = \e -> ((enforce stmts) . (listEnforce vars) . (listDo (\(t,n) -> addSymbol (n, t)) args) . (addSymbol (name, (Function (map fst args) ret))) . (setScope (Local name))) e
	getType (FD ret name args vars stmts) = \_ -> Function (map fst args) ret
	
instance TypeCheck Exp where
	getType (Int _) = const Int_
	getType (Bool _) = const Bool_
	getType (Tuple a b) = \e -> Tuple_ (getType a e) (getType b e)
	getType (Id name) = fst . getSymbol name
	getType EmptyList = const (List_ (Generic_ "_list")) -- FIXME: what to do here?
	-- FIXME: find all cases
	getType (ExpOp_ AppCons a b) = getType b
	getType (ExpOp_ o a b) = if elem o [Equals, LessEq, MoreEq, NotEq, Less, More] then const Bool_ else const Int_
	getType (Op1_ o a) = const Int_ -- FIXME: sometimes it's bool
	getType (FunCall (name, args)) = \e -> case fst (getSymbol name e) of -- FIXME: correct, different scope?
		Function v r -> r

	enforce (Int _) = id
	enforce (Bool _) = id
	enforce EmptyList = id
	-- TODO: make cases for ints and bools. depends on the operator
	enforce (ExpOp_ AppCons a b) = \e -> (unify (List_ (getType a e)) (getType b e) . enforce a . enforce b) e
	enforce (ExpOp_ o a b) = \e -> (unify (getType a e) (getType b e) . enforce a . enforce b) e

	enforce (Op1_ o a) = \e -> unify (getType a e) Int_ e
	enforce (Tuple a b) = enforce a . enforce b
	enforce (Id name) = snd . getSymbol name 
	-- TODO: check if number of arguments is the same
	enforce (FunCall (name, args)) = \e -> case fst (getSymbol name e) of
		Function v r -> foldl (\x (a,b)-> unify a b x) e (zip v (map (\x -> getType x e) args))

instance TypeCheck Stmt where	
	enforce (If cond stmt) = (\e -> unify (getType cond e) Bool_ e) . enforce stmt
	enforce (IfElse cond stmt1 stmt2) = (\e -> unify (getType cond e) Bool_ e) . enforce stmt1 . enforce stmt2
	enforce (Assign id exp) = \e -> unify (fst (getSymbol id e)) (getType exp e) e
	enforce (While cond stmt) = (\e -> unify (getType cond e) Bool_ e) . enforce stmt
	enforce (Seq stmt) = \e -> foldl (\x y -> enforce y x) e stmt
	enforce (FunCall_ funCall) = enforce (FunCall funCall) -- call the Exp enforcer
	enforce (Return a) = case a of 
		Just exp -> (\e -> unify (getType exp e) (getRet e) e)  . (enforce exp)
		Nothing -> \e -> unify Void (getRet e) e
		where getRet = \e -> case getLocalFunc e of 
				(Function a r, _) -> r
