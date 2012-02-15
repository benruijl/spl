module AST where

type Id = String

data Prog = Decl
data Decl = VarDecl | FunDecl

type VarDecl = ((Type, Id), Exp)

type FArgs = [(Type, Id)]
type ActArgs = [Exp]
type FunCall = (Id,[ActArgs])

-- TODO: fix, left recursive
data Exp = Id Id | Op2_ Op2 Exp Exp | Op1 Exp | Int Int | Bool Bool | FunCall | List | Tuple Exp Exp deriving (Show) -- refers to the concrete value assigned to a 'Type'

--data Exp = Id Id Exp' | Op1 Exp' | Int Exp' | Bool Exp' | FunCall Exp' | List Exp' | Tuple Exp' Exp' Exp' deriving (Show)
--data Exp' = epsilon | Op2 Exp Exp'


data RetType = Type | Void
data Type = Id_ | Int_ | Bool_ | Tuple_ Type Type | List_ Type  deriving (Show)

data Stmt = If Exp Stmt | IfElse Exp Stmt Stmt | While Exp Stmt | Assign Id Exp | FunCall_ FunCall | Return Exp deriving (Show)


data Op1 = Negate | UnitaryMinus deriving (Show)
data Op2 = Add | Sub | Mult | Div | Mod | Equals | Less | More | LessEq | MoreEq | NotEq | And | Or | AppCons deriving (Show)
