module AST where

type Id = String

type Prog = [Decl]
data Decl = VarDecl VarDecl | FunDecl FunDecl

data VarDecl = VD Type Id Exp
data FunDecl = FD RetType Id FArgs [VarDecl] [Stmt] -- you already declared Stmt = [Stmt] then you don't have to specify the fact that it's a list here

type FArgs = [(Type, Id)]
type ActArgs = [Exp]
type FunCall = (Id, ActArgs)

-- AST structure, not the same as the grammar structure
data Exp = ExpOp_ ExpOp Exp Exp | Int Int | Id Id  | Op1 Exp | Bool Bool | FunCall FunCall | EmptyList | Tuple Exp Exp deriving (Show)

data RetType = Type Type | Void deriving (Show)
data Type = Id_ | Int_ | Bool_ | Tuple_ Type Type | List_ Type

data Stmt = List [Stmt] | If Exp Stmt | IfElse Exp Stmt Stmt | While Exp Stmt | Assign Id Exp | FunCall_ FunCall | Return Exp

data Op1 = Negate | UnitaryMinus deriving (Show)
data ExpOp = Add | Sub | Mod | Equals | Less | More | LessEq | MoreEq | NotEq | And | Or | AppCons | Mult | Div deriving (Show)

instance Show Type where
	show Id_ = ""
	show Int_ = "int"
	show Bool_ = "bool"
	show (Tuple_ x y) = "(" ++ show x ++ "," ++ show y ++ ")"
	show (List_ x) = "[" ++ show x ++ "]"

instance Show Decl where
    show (VarDecl varDecl) = show varDecl
    show (FunDecl funDecl) = show funDecl

instance Show VarDecl where
    show (VD varType ident ass) = show varType ++ " " ++ ident ++ " = " ++ show ass ++ ";"
	
instance Show FunDecl where
	show (FD retType ident fArgs varDecl stmt) = show retType ++ " " ++ show ident ++ "(" ++ show fArgs ++ ")" ++ "\n{" ++ show varDecl ++ show stmt ++ "\n}"
	
instance Show Stmt where 
    --show (List [Stmt]) = show (
	show (If exp stmt) = "If(" ++ show exp ++ ")" ++ show stmt ++ ";" -- we didn't take {} into account yet
	show (IfElse exp stmt1 stmt2) = "If(" ++ show exp ++ ")" ++ show stmt1 ++ "\nelse" ++ show stmt2 ++ ";"
	show (While exp stmt) = "While(" ++ show exp ++ ")\n{" ++ show stmt ++ "\n}"
--	show (Assign ident exp) = show ident "=" show exp ++ ";"
	show (FunCall_ funCall) = show funCall ++ ";"
--  show (Return exp) = "return" ++ show exp ++ ";" 	

prettyPrint :: Prog -> [String]
prettyPrint x = map show x
