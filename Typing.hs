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
	
-- FIXME: remove these routines
getReducedTypeForName :: Id -> TC Type
getReducedTypeForName id = (\e -> (getReducedType (getSymbol id e) (setScope (Local id) e), e))

getReducedTypeInFn :: Id -> Type -> Env -> Type
getReducedTypeInFn fn tp = getReducedType tp . setScope (Local fn)

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

fullEnforce :: Prog -> Env -> Env
fullEnforce p e = snd $ iter enforce p e

listDo :: (a -> Env -> Env) -> [a] -> Env -> Env
listDo a l = \e -> foldl (\x y -> a y x) e l

type TC a = Env -> (a, Env)

yield :: a -> Env -> (a, Env)
yield t e = (t, e)

infixl 5 >->
(>->) :: (Env -> (a, Env)) -> (a -> b) -> (Env -> (b, Env))
(>->) p k e =
  case p e of
    (a,e') -> (k a, e')

-- mutate environment
infixl 5 >-->
(>-->) ::  (Env -> Env) -> (Env -> (a, Env)) -> (Env -> (a, Env))
(>-->) k p e =
  case p e of
    (a,e') -> (a, k e')
    
infixl 6 #
(#) :: TC a -> TC b -> TC (a,b)
(#) a b e =
  case a e of
    (c, e') ->
      case b e' of
        (q, e'') -> ((c,q), e'')
   
-- unify 
-- FIXME: is the returned type correct? also, check the infix
infixl 7 !~!
(!~!) :: TC Type -> TC Type -> TC Type
(!~!) a b e =  
  case (a # b) e of
    ((x, y), e') -> (x, unify x y e')
    
iter :: (a -> Env -> (b, Env)) -> [a] -> Env -> ([b], Env)
iter f [] = yield []
iter f (l:ls) = f l # iter f ls >-> (\(a,b) -> a:b)

class TypeCheck a where
	enforce :: a -> Env -> (Type, Env)

instance TypeCheck Decl where
	enforce (FunDecl f) = enforce f
	enforce (VarDecl v) = enforce v
	
instance TypeCheck VarDecl where
	-- TODO: remove e
	-- FIXME: always show reduced type?
	enforce (VD t name exp) = \e -> if getScope e == Global then (setScope Global >--> enf . setScope (Local name) . addSymbol (name, t)) e else (enf . addSymbol (name, t)) e
		where
		enf = yield t !~! enforce exp

instance TypeCheck FunDecl where
	-- TODO: add all function definitions at the start, so they can be called from anywhere
	-- FIXME: update the global symbol definition with the constraints?
	-- TODO: watch out, # is from left to right
	enforce (FD ret name args vars stmts) = (setScope Global >--> iter enforce vars # enforce stmts >-> (\_ -> ret)) . listDo (\(t,n) -> addSymbol (n, t)) args .  setScope (Local name) . addSymbol (name, (Function (map fst args) ret))

instance TypeCheck Exp where
	enforce (Int _) = yield Int_
	enforce (Bool _) = yield Bool_
	enforce EmptyList = uniqueVar >-> (\x -> List_ x)
	enforce (ExpOp_ AppCons a EmptyList) = enforce a >-> (\x -> List_ x) -- always accept empty list
	enforce (ExpOp_ AppCons a b) = (enforce a >-> (\x -> List_ x)) !~! enforce b

	-- FIXME	
	enforce (ExpOp_ o a b) = if elem o [And, Or] then (enforce a) !~! (yield Bool_) #  (enforce b) !~! (yield Bool_) >-> (\_ -> Bool_) else yield Int_ -- enforce a !~! yield Int_ #  enforce b !~! yield Int_ >-> (\_ -> if elem o [Add, Sub, Mul, Div, Mod] then Bool else Int)
	enforce (Op1_ UnitaryMinus a) =  enforce a !~! yield Int_
	enforce (Op1_ Negate a) = enforce a !~! yield Bool_
	enforce (Tuple a b) = enforce a # enforce b >-> (\(x, y) -> Tuple_ x y)
	enforce (Id name) = getReducedTypeForName name -- FIXME: is this correct?
	-- FIXME: unify with global symbol definition?
	-- FIXME: code is a mess
	enforce (FunCall (name, args)) = \e -> case getSymbol name e of
		Function v r -> (setScope (getScope e) >--> getReturnType . returnEnforce . listEnforce . buildList . (setScope FunctionCall >--> iter enforce args) . (listDo renameUnique (r:v)) . clearFunc . argCheck) e
			where 
			getReturnType = \e -> (getReducedType (transformTypes r e) e, e)
			returnEnforce = \e -> unify (getReducedTypeInFn name r e) (transformTypes r e) e
			listEnforce = \(z, e') -> listDo (\(a,b)-> unify a b) z e'
			argCheck = if length args == length v then id else error "Number of arguments does not match"
			buildList = \(z,e') -> (zip z (map (\x -> transformTypes (getReducedTypeInFn name x e') e') v), e')
		_ -> error $ "Not a function: " ++ name	

instance TypeCheck Stmt where	
	enforce (If cond stmt) = enforce cond !~! yield Bool_  # enforce stmt >-> (\_ -> Undefined)
	enforce (IfElse cond stmt1 stmt2) = enforce cond !~! yield Bool_  # enforce stmt1 # enforce stmt2 >-> (\_ -> Undefined)
	enforce (Assign id exp) = (\e -> (getSymbol id e, e)) !~! enforce exp >-> (\_ -> Undefined)
	enforce (While cond stmt) =  enforce cond !~! yield Bool_ # enforce stmt >-> (\_ -> Undefined)
	enforce (Seq stmt) = iter enforce stmt >-> (\_ -> Undefined)
	enforce (FunCall_ funCall) = enforce (FunCall funCall) -- call the Exp enforcer
	enforce (Return a) = case a of 
		Just exp -> getRet !~! enforce exp
		Nothing -> getRet !~! yield Void
		where getRet = \e -> case getLocalFunc e of 
				(Function a r) -> yield r e -- TODO: return reduced type?
				_ -> error "Not in function" -- should not happen 
