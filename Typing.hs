module Typing where

import AST
import Combinators
import Data.List
import qualified Data.Map as Map

-- Env = (fresh var count, global symb table, local symb table, funcall symb table, scope)
data Scope = Global | Local Id | FunctionCall deriving (Show, Eq)
type SymbolTable = ([(Type, Type)], [(Id, Type)])
type SymbolRenameTable = ([(Type, Type)], Map.Map Type Type)
type Env = (Int, Map.Map Id Type, Map.Map Id SymbolTable, SymbolRenameTable, Scope) 
type TypeChecker =  Env -> (Type, Env)

showEnv :: Env -> String
showEnv (i,m,l,f,c) = "Fresh var count: " ++ show i ++ "\nGlobal symbol table:" ++ show m ++ "\nLocal symbol table:\n" ++ showMap l ++ "\nFunction call symbol table: " ++ show f ++ "\nCurrent scope: " ++ show c

showMap :: (Show a, Show b) => Map.Map a b -> String
showMap m = unlines $ map (\(a, b) -> show a ++ ": " ++ show b) (Map.toList m)

-- Renames generic types to unique names and puts them in a map
renameUnique :: Type -> Env -> Env
renameUnique (Function v r) = listDo renameUnique (r:v)
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

getScope :: Env -> Scope
getScope = \e@(i,m,l,f,c) -> c

-- Sets the current scope
setScope :: Scope -> Env -> Env
setScope s = \e@(i,m,l,f,c) -> (i, m, l, f, s)

-- Add variable or function to the global symbol table
addGlob :: (Id, Type) -> Env -> Env
addGlob (id, t) = \(i,m,l,f,c) -> (i, Map.insert id t m,l,f,c)

addLoc :: SymbolTable -> Env -> Env
addLoc (t, d) = \(i,g,l,f, Local id) -> (i,g, Map.insertWith (\(a,b) (ot, od) -> (a++ot, b++od)) id (t,d) l, f, Local id)

-- Adds symbol types in the function scope
addFunc :: (Type, Type) -> Env -> Env
addFunc t = \(i,g,l,(ot, od),c) -> (i,g,l,(t : ot, od),c)

addSymbol :: (Id, Type) -> Env -> Env
addSymbol s e@(i, m, l, f, Global) = addGlob s e
addSymbol s e@(i, m, l, f, Local id) = addLoc ([], [s]) e

addSymbolType :: (Type, Type) -> Env -> Env
addSymbolType s e@(i, m, l, f, Global) = error "Not allowed to add global map"
addSymbolType s e@(i, m, l, f, Local id) = addLoc ([s], []) e
addSymbolType s e@(i, m, l, f, FunctionCall) = addFunc s e

-- TODO: no getSymbol for FunctionCall
getSymbol :: Id -> Env -> Type
getSymbol id e@(i, s, l, f, Global) = case Map.lookup id s of
	Just t -> t
	Nothing -> error $ "Undefined variable or function '" ++ id ++ "', " ++ showEnv e
getSymbol id e@(i, m, l, f, Local fid) = case Map.lookup fid l of 
	Just (_, s) ->  case find (\(i,t) -> i == id) s of
		Just (i, t) -> t
		Nothing -> getSymbol id (i, m, l, f, Global) -- search globally
	Nothing -> getSymbol id (i, m, l, f, Global)

getSymbolType t e@(_, _, _, _, c) = getSymbolType' t c e

getSymbolType' :: Type -> Scope -> Env-> Maybe (Type, Type)
getSymbolType' id (Local fid) e@(_, _, l, _, _) = case Map.lookup fid l of 
	Just (s,_) -> case find (\(i,t) -> i == id) s of
		Just (i, t) -> Just (i, t)
		Nothing -> Nothing
	Nothing -> Nothing
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
getReducedType (Function a r) = \e -> Function (map (\x -> getReducedType x e) a) (getReducedType r e)
getReducedType (List_ a) = \e -> List_ (getReducedType a e)
getReducedType (Tuple_ a b) = \e -> Tuple_ (getReducedType a e) (getReducedType b e)
getReducedType id = \e -> case getSymbolType id e of
	Just (_, t) -> getReducedType t e
	Nothing -> id

-- Looks the symbol up in a map and transforms it
-- TODO: may be better to save updated local function definition
transformTypes :: Type -> Env -> Type
transformTypes (List_ a) = \e -> List_ (transformTypes a e)
transformTypes (Tuple_ a b) = \e -> Tuple_ (transformTypes a e) (transformTypes b e)
transformTypes t = \e@(i, m, l, (_, s), c) -> case Map.lookup t s of
	Just k -> k
	Nothing -> t
	
-- TODO: incorporate in combinators
getReducedTypeForName :: Id -> TC Type
getReducedTypeForName id = (\e -> (getReducedType (getSymbol id e) (setScope (Local id) e), e))

-- Gets the function type the checker is currently in
getCurrentFuncType :: Env -> Type
getCurrentFuncType e@(i,m,n,l,Local id) = getSymbol id e
getCurrentFuncType _ =  error $ "Not in function"

getReducedTypeInFn :: Id -> Type -> Env -> Type
getReducedTypeInFn fn tp = getReducedType tp . setScope (Local fn)

updateDef :: Id -> Env -> Env
updateDef id = \e -> addGlob (id, fst $ getReducedTypeForName id e) e

cleanEnv :: Env
cleanEnv = (0, Map.empty, Map.empty, ([], Map.empty), Global)

clearFunc :: Env -> Env
clearFunc (i, m, l, f, c) = (i, m, l, ([],Map.empty), c)

-- Builds a global environment from a program
buildEnv :: Prog -> Env
buildEnv p = foldl (\x y -> case y of 
		 (VarDecl (VD t name _)) -> addSymbol (name, t) x
		 (FunDecl (FD ret name args vars stmts)) -> addSymbol (name, Function (map fst args) ret) x)
		(defaultFunctions cleanEnv) p

-- Adds the default functions
-- Assumes the Scope is set to Global
defaultFunctions :: Env -> Env
defaultFunctions = listDo addSymbol [hd, tl, isEmpty, fst, snd, print, printChar, free]
	where
	hd = ("head", Function [List_ (Generic_ "t")] (Generic_ "t"))
	tl = ("tail", Function [List_ (Generic_ "t")] (List_ (Generic_ "t")))
	isEmpty = ("empty", Function [List_ (Generic_ "t")] Bool_)
	fst = ("fst", Function [Tuple_ (Generic_ "t") (Generic_ "v")] (Generic_ "t"))
	snd = ("snd", Function [Tuple_ (Generic_ "t") (Generic_ "v")] (Generic_ "t"))
	print = ("print", Function [Generic_ "t"] Void)
	printChar = ("printChar", Function [Generic_ "t"] Void)
	free = ("free", Function [Generic_ "t"] Void)

-- Type check an entire program
progTypeCheck :: Prog -> Env
progTypeCheck p = snd $ iter typeCheck p (buildEnv p)

listDo :: (a -> Env -> Env) -> [a] -> Env -> Env
listDo a l = \e -> foldl (\x y -> a y x) e l

type TC a = Env -> (a, Env)

-- unify
infixl 7 !~!
(!~!) :: TC Type -> TC Type -> TC Type
(!~!) a b e =  
  case (a # b) e of
    ((x, y), e') -> (x, unify x y e')

class TypeCheck a where
	typeCheck :: a -> Env -> (Type, Env)

instance TypeCheck Decl where
	typeCheck (FunDecl f) = typeCheck f
	typeCheck (VarDecl v) = typeCheck v
	
instance TypeCheck VarDecl where
	-- TODO: remove e
	typeCheck (VD t name exp) = \e -> if getScope e == Global then (setScope Global >--> updateDef name >--> enf . setScope (Local name) . addSymbol (name, t)) e else (enf . addSymbol (name, t)) e
		where
		enf = yield t !~! typeCheck exp

instance TypeCheck FunDecl where
	-- TODO: add all function definitions at the start, so they can be called from anywhere
	typeCheck (FD ret name args vars stmts) = (setScope Global >--> updateDef name >--> iter typeCheck vars # typeCheck stmts >-> (\_ -> ret)) . listDo (\(t,n) -> addSymbol (n, t)) args .  setScope (Local name) . addSymbol (name, (Function (map fst args) ret))

instance TypeCheck Exp where
	typeCheck (Int _) = yield Int_
	typeCheck (Bool _) = yield Bool_
	typeCheck EmptyList = uniqueVar >-> (\x -> List_ x)
	typeCheck (ExpOp_ AppCons a EmptyList) = typeCheck a >-> (\x -> List_ x) -- always accept empty list
	typeCheck (ExpOp_ AppCons a b) = (typeCheck a >-> (\x -> List_ x)) !~! typeCheck b
	typeCheck (ExpOp_ o a b) = if elem o [And, Or] then typeCheck a !~! yield Bool_ #  typeCheck b !~! yield Bool_ >-> (\_ -> Bool_) else typeCheck a !~! yield Int_ #  typeCheck b !~! yield Int_ >-> (\_ -> if elem o [Add, Sub, Mul, Div, Mod] then Int_ else Bool_)
	typeCheck (Op1_ UnitaryMinus a) =  typeCheck a !~! yield Int_
	typeCheck (Op1_ Negate a) = typeCheck a !~! yield Bool_
	typeCheck (Tuple a b) = typeCheck a # typeCheck b >-> (\(x, y) -> Tuple_ x y)
	typeCheck (Id name) = getReducedTypeForName name -- FIXME: is this correct?
	-- FIXME: too complicated
	typeCheck (FunCall (name, args)) = \e -> case getSymbol name e of
		Function v r -> (setScope (getScope e) >--> getReturnType . returntypeCheck . listtypeCheck . buildList . (setScope FunctionCall >--> iter typeCheck args) . (listDo renameUnique (r:v)) . clearFunc . argCheck) e
			where 
			getReturnType = \e -> (getReducedType (transformTypes r e) e, e)
			returntypeCheck = \e -> unify (getReducedTypeInFn name r e) (transformTypes r e) e
			listtypeCheck = \(z, e') -> listDo (\(a,b)-> unify a b) z e'
			argCheck = if length args == length v then id else error "Number of arguments does not match"
			buildList = \(z,e') -> (zip z (map (\x -> transformTypes (getReducedTypeInFn name x e') e') v), e')
		_ -> error $ "Not a function: " ++ name	

instance TypeCheck Stmt where	
	typeCheck (If cond stmt) = typeCheck cond !~! yield Bool_  # typeCheck stmt >-> (\_ -> Undefined)
	typeCheck (IfElse cond stmt1 stmt2) = typeCheck cond !~! yield Bool_  # typeCheck stmt1 # typeCheck stmt2 >-> (\_ -> Undefined)
	typeCheck (Assign id exp) = (\e -> (getSymbol id e, e)) !~! typeCheck exp >-> (\_ -> Undefined)
	typeCheck (While cond stmt) =  typeCheck cond !~! yield Bool_ # typeCheck stmt >-> (\_ -> Undefined)
	typeCheck (Seq stmt) = iter typeCheck stmt >-> (\_ -> Undefined)
	typeCheck (FunCall_ funCall) = typeCheck (FunCall funCall) -- call the Exp typeChecker
	typeCheck (Return a) = case a of 
		Just exp -> getRet !~! typeCheck exp
		Nothing -> getRet !~! yield Void
		where getRet = \e -> case getCurrentFuncType e of 
				(Function a r) -> yield r e -- TODO: return reduced type?
				_ -> error "Not in function" -- should not happen 
