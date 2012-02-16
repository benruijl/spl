module AST where

type Id = String

data Prog = Decl deriving (Show)
data Decl = VarDecl | FunDecl deriving (Show)

-- make a type?
data VarDecl = VD Type Id Exp deriving (Show)
data FunDecl = FD RetType Id FArgs [VarDecl] [Stmt] deriving (Show)

type FArgs = [(Type, Id)]
type ActArgs = [Exp]
type FunCall = (Id,[ActArgs])

data Exp = Id Id | Op2_ Op2 Exp Exp | Op1 Exp | Int Int | Bool Bool | FunCall | EmptyList | Tuple Exp Exp deriving (Show) -- refers to the concrete value assigned to a 'Type'


data RetType = Type Type | Void deriving (Show)
data Type = Id_ | Int_ | Bool_ | Tuple_ Type Type | List_ Type  deriving (Show)

data Stmt = List [Stmt] | If Exp Stmt | IfElse Exp Stmt Stmt | While Exp Stmt | Assign Id Exp | FunCall_ FunCall | Return Exp deriving (Show)


data Op1 = Negate | UnitaryMinus deriving (Show)
data Op2 = Add | Sub | Mult | Div | Mod | Equals | Less | More | LessEq | MoreEq | NotEq | And | Or | AppCons deriving (Show)
