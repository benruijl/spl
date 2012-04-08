module Typing where

import AST
import Data.List
import qualified Data.Map as Map

-- Env = (fresh var count, global symb. table, local symb table, funcall symb table, scope)
data Scope = Global | Local Id | FunctionCall deriving (Show, Eq)
type SymbolTable = ([(Type, Type)], [(Id, Type)])
type SymbolRenameTable = ([(Type, Type)], Map.Map Type Type)
type Env = (Int, SymbolTable, Map.Map Id SymbolTable, SymbolRenameTable, Scope) 
type TypeChecker =  Env -> (Type, Env)

showEnv :: Env -> String
showEnv (i,m,l,f,c) = "Fresh var count: " ++ show i ++ "\nGlobal symbol table:" ++ show m ++ "\nLocal symbol table:\n" ++ showMap l ++ "\nFunction call symbol table: " ++ show f ++ "\nCurrent scope: " ++ show c

showMap :: (Show a, Show b) => Map.Map a b -> String
showMap m = unlines $ map (\(a, b) -> show a ++ ": " ++ show b) (Map.toList m)

-- TODO: only applies for function calls
renameUnique :: Type -> Env -> Env
renameUnique (Tuple_ a b) = (renameUnique b . renameUnique a)
renameUnique (List_ a) = renameUnique a
renameUnique a@(Generic_ _) = \e@(i,m,l,(b, s),c) -> case Map.lookup a s of
	Just _ -> e
	Nothing -> (i + 1,m,l,(b, Map.insert a (Generic_ ("_a" ++ show i)) s),c)
renameUnique _ = id

uniqueVar :: Env -> (Type, Env)
uniqueVar = \e@(i,m,l,(b, s),c) -> (Generic_ ("_a" ++ show i), (i+1,m,l,(b, s),c))

-- check if the type a is included in type b
isIncluded :: Type -> Type -> Bool
isIncluded k (Tuple_ a b) = isIncluded k a || isIncluded k b
isIncluded k (List_ a) = isIncluded k a
isIncluded a b = a == b

unify :: Type -> Type -> Env -> Env
unify (List_ a) (List_ b) = unify a b
unify (Tuple_ a b) (Tuple_ d e) = unify a d . unify b e
unify (Generic_ a) (Generic_ b) = if (a == b) then id else addMap (Generic_ a) (Generic_ b) -- TODO: is this safe?
unify (Generic_ a) b = \e -> if isIncluded (Generic_ a) b then error $ "Cannot unify types: " ++ a ++ " and " ++ show b ++ showEnv e else addMap (Generic_ a) b e
unify a (Generic_ b) = unify (Generic_ b) a
unify a b = \e -> if a == b then e else error $ "Cannot unify types: " ++ show a ++ " and " ++ show b ++ "\nDump:\n" ++ showEnv e

-- Gets the function type the checker is currently in
-- TODO: rename
getLocalFunc :: Env -> Type
getLocalFunc e@(i,m,n,l,Local id) = getSymbol id e
getLocalFunc _ =  error $ "Not in function"

getScope :: Env -> Scope
getScope = \e@(i,m,l,f,c) -> c

-- Sets the current scope
setScope :: Scope -> Env -> Env
setScope s = \e@(i,m,l,f,c) -> (i, m, l, f, s)

-- Add variable or function to the global symbol table
addGlob :: SymbolTable -> Env -> Env
addGlob (t, d) = \(i,(ot, od),l,f,c) -> (i,(t ++ ot, d ++ od),l,f,c)

addLoc :: SymbolTable -> Env -> Env
addLoc (t, d) = \(i,g,l,f, Local id) -> (i,g, Map.insertWith (\(a,b) (ot, od) -> (a++ot, b++od)) id (t,d) l, f, Local id)

-- Adds symbols in the function scope
-- FIXME: a variable map is never added. assess if required?
addFunc :: SymbolTable -> Env -> Env
addFunc (t, d) = \(i,g,l,(ot, od),c) -> (i,g,l,(t ++ ot, od),c)

addSymbol :: (Id, Type) -> Env -> Env
addSymbol s e@(i, m, l, f, Global) = addGlob ([], [s]) e
addSymbol s e@(i, m, l, f, Local id) = addLoc ([], [s]) e
addSymbol s e@(i, m, l, f, FunctionCall) = addFunc ([], [s]) e

addSymbolType :: (Type, Type) -> Env -> Env
addSymbolType s e@(i, m, l, f, Global) = addGlob ([s], []) e
addSymbolType s e@(i, m, l, f, Local id) = addLoc ([s], []) e
addSymbolType s e@(i, m, l, f, FunctionCall) = addFunc ([s], []) e

-- TODO: no getSymbol for FunctionCall
getSymbol :: Id -> Env -> Type
getSymbol id e@(i, (_, s), l, f, Global) = case find (\(i,t) -> i == id) s of
	Just (i, t) -> t
	Nothing -> error $ "Undefined variable or function '" ++ id ++ "', " ++ showEnv e
getSymbol id e@(i, m, l, f, Local fid) = case Map.lookup fid l of 
	Just (_, s) ->  case find (\(i,t) -> i == id) s of
		Just (i, t) -> t
		Nothing -> getSymbol id (i, m, l, f, Global) -- search globally
	Nothing -> getSymbol id (i, m, l, f, Global)

getSymbolType t e@(_, (s, _), _, _, c) = getSymbolType' t c e

getSymbolType' :: Type -> Scope -> Env-> Maybe (Type, Type)
getSymbolType' id Global e@(_, (s, _), _, _, _) = find (\(i,t) -> i == id) s
getSymbolType' id (Local fid) e@(_, _, l, _, _) = case Map.lookup fid l of 
	Just (s,_) -> case find (\(i,t) -> i == id) s of
		Just (i, t) -> Just (i, t)
		Nothing -> getSymbolType' id Global e
	Nothing ->  getSymbolType' id Global e
getSymbolType' id FunctionCall e@(_, _, _, (s, _), _) = case find (\(i,t) -> i == id) s of
	Just (i, t) -> Just (i, t)
	Nothing -> getSymbolType' id (Local "") e -- FIXME: it matter what the function is, this will fail!

-- Adds a map from a generic to another type
addMap :: Type -> Type -> Env -> Env
addMap a@(Generic_ _) b = \e -> case getSymbolType a e of
	Just (_, c) ->  unify b c e -- Add a map from b to c
	Nothing -> addSymbolType (a, b) e
addMap a b = \e -> if a == b then e else error $ "Cannot unify types: " ++ show a ++ " and " ++ show b ++ "\nDump:\n" ++ showEnv e

-- Gets the final type of a symbol
getReducedType :: Type -> Env -> Type
getReducedType (List_ a) = \e -> List_ (getReducedType a e)
getReducedType (Tuple_ a b) = \e -> Tuple_ (getReducedType a e) (getReducedType b e)
getReducedType id = \e -> case getSymbolType id e of
	Just (_, t) -> getReducedType t e
	Nothing -> id

-- Looks the symbol up in a map and transforms it
-- TODO: may be better to save local function definition
transformTypes :: Type -> Env -> Type
transformTypes (List_ a) = \e -> List_ (transformTypes a e)
transformTypes (Tuple_ a b) = \e -> Tuple_ (transformTypes a e) (transformTypes b e)
transformTypes t = \e@(i, m, l, (_, s), c) -> case Map.lookup t s of
	Just k -> k
	Nothing -> t

cleanEnv :: Env
cleanEnv = (0, ([],[]), Map.empty, ([], Map.empty), Global)

clearFunc :: Env -> Env
clearFunc (i, m, l, f, c) = (i, m, l, ([],Map.empty), c)

-- Builds a global environment from a program
-- TODO: Should this be done?
buildEnv :: Prog -> Env
buildEnv p = foldl (\x y -> case y of 
		 (VarDecl (VD t name _)) -> addSymbol (name, t) x
		 (FunDecl (FD ret name args vars stmts)) -> addSymbol (name, Function (map fst args) ret) x)
		(defaultFunctions (0, ([],[]), Map.empty, ([],Map.empty), Global)) p

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

-- Note: getType should not modify the state
getType :: Exp -> Env -> Type
getType (Int _) = const Int_
getType (Bool _) = const Bool_
getType (Tuple a b) = \e -> Tuple_ (getType a e) (getType b e)
getType (Id name) = (\e -> getReducedType (getSymbol name e) e) . setScope (Local name) 
getType EmptyList = const (List_ (Generic_ "__EL")) -- FIXME: what to do here?
getType (ExpOp_ AppCons a b) = \e -> List_ (getType a e) -- TODO: done to circumvent problems with empty list
getType (ExpOp_ o a b) = if elem o [Add, Sub, Mul, Div, Mod] then const Int_ else const Bool_
getType (Op1_ UnitaryMinus _) = const Int_
getType (Op1_ Negate _) = const Bool_
-- FIXME: getType FunCall only valid when called after enforce!
getType (FunCall (name, args)) = \e -> case getSymbol name e of
	Function v r -> ((\e2 -> getReducedType (transformTypes r e2) e2) . setScope FunctionCall) e

class TypeCheck a where
	enforce :: a -> Env -> Env

instance TypeCheck Decl where
	enforce (FunDecl f) = enforce f
	enforce (VarDecl v) = enforce v
	
instance TypeCheck VarDecl where
	enforce (VD t name exp) = \e -> if (getScope e == Global) then (setScope Global . (\ne -> unify t (getType exp ne) ne) . enforce exp . setScope (Local name) . addSymbol (name, t)) e else ( (\ne -> unify t (getType exp ne) ne) . enforce exp . addSymbol (name, t)) e

instance TypeCheck FunDecl where
	-- TODO: add all function definitions at the start, so they can be called from anywhere
	-- FIXME: update the global symbol definition with the constraints?
	enforce (FD ret name args vars stmts) = \e -> (setScope Global . enforce stmts . listEnforce vars . (listDo (\(t,n) -> addSymbol (n, t)) args) . (setScope (Local name)) . (addSymbol (name, (Function (map fst args) ret)))) e

instance TypeCheck Exp where
	enforce (Int _) = id
	enforce (Bool _) = id
	enforce EmptyList = id
	 -- FIXME: is this correct?
	enforce (ExpOp_ AppCons a EmptyList) = enforce a -- always accept
	enforce (ExpOp_ AppCons a b) = \e -> (unify (List_ (getType a e)) (getType b e) . enforce a . enforce b) e
	enforce (ExpOp_ o a b) = \e -> ((if elem o [And, Or] then unify (getType a e) Bool_ else unify (getType a e) Int_) . unify (getType a e) (getType b e) . enforce a . enforce b) e
	enforce (Op1_ UnitaryMinus a) = \e -> unify (getType a e) Int_ e
	enforce (Op1_ Negate a) = \e -> unify (getType a e) Bool_ e
	enforce (Tuple a b) = enforce a . enforce b
	enforce (Id name) = seq (getSymbol name)  -- check if variable is defined
	-- FIXME: unify with global symbol definition?
	enforce (FunCall (name, args)) = \e -> case getSymbol name e of
		Function v r -> (setScope (getScope e) . (\env -> listDo (\(a,b)-> unify a b) (buildList env) env) . collectReturnType . setScope FunctionCall . (listDo renameUnique (r:v)) . clearFunc . argCheck) e
			where
			argCheck = if length args == length v then id else error "Number of arguments does not match"
			buildList = \env -> zip (map (\x -> transformTypes x env) v) (map (\x -> getType x e) args) -- use e and not env, so the scope is Local
			
			-- FIXME: kind of a hack
			collectReturnType = \e2 -> unify (transformTypes r e2) (((getReducedType r) . setScope (Local name)) e2) e2
		_ -> error $ "Not a function: " ++ name		

instance TypeCheck Stmt where	
	enforce (If cond stmt) = (\e -> unify (getType cond e) Bool_ e) . enforce stmt
	enforce (IfElse cond stmt1 stmt2) = (\e -> unify (getType cond e) Bool_ e) . enforce stmt1 . enforce stmt2
	enforce (Assign id exp) = \e -> unify (getSymbol id e) (getType exp e) e
	enforce (While cond stmt) = (\e -> unify (getType cond e) Bool_ e) . enforce stmt
	enforce (Seq stmt) = \e -> foldl (\x y -> enforce y x) e stmt
	enforce (FunCall_ funCall) = enforce (FunCall funCall) -- call the Exp enforcer
	enforce (Return a) = case a of 
		Just exp -> (\e -> unify (getType exp e) (getRet e) e)  . (enforce exp)
		Nothing -> \e -> unify Void (getRet e) e
		where getRet = \e -> case getLocalFunc e of 
				(Function a r) -> r
				_ -> error "Not in function" -- should not happen
