module AST where

type Id = String

type Prog = [Decl]
data Decl = VarDecl VarDecl | FunDecl FunDecl deriving (Show)

-- make a type?
data VarDecl = VD Type Id Exp
data FunDecl = FD RetType Id FArgs [VarDecl] [Stmt] deriving (Show)

type FArgs = [(Type, Id)]
type ActArgs = [Exp]
type FunCall = (Id, ActArgs)

-- AST structure, not the same as the grammar structure
data Exp = ExpOp_ ExpOp Exp Exp | Int Int | Id Id  | Op1 Exp | Bool Bool | FunCall FunCall | EmptyList | Tuple Exp Exp deriving (Show)


data RetType = Type Type | Void deriving (Show)
data Type = Id_ | Int_ | Bool_ | Tuple_ Type Type | List_ Type

data Stmt = List [Stmt] | If Exp Stmt | IfElse Exp Stmt Stmt | While Exp Stmt | Assign Id Exp | FunCall_ FunCall | Return Exp deriving (Show)


data Op1 = Negate | UnitaryMinus deriving (Show)
data ExpOp = Add | Sub | Mod | Equals | Less | More | LessEq | MoreEq | NotEq | And | Or | AppCons | Mult | Div deriving (Show)

instance Show Type where
	show Id_ = ""
	show Int_ = "int"
	show Bool_ = "bool"
	show (Tuple_ x y) = "(" ++ show x ++ "," ++ show y ++ ")"
	show (List_ x) = "[" ++ show x ++ "]"

instance Show VarDecl where
    show (VD varType id ass) = show varType ++ " " ++ id ++ " = " ++ show ass ++ ";"

prettyPrint :: Prog -> [String]
prettyPrint x = map show x

