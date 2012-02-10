module AST where

type Id = String

data Prog = Decl
data Decl = VarDecl | FunDecl

type VarDecl = ((Type, Id), Exp)

type FArgs = [(Type, Id)]
type ActArgs = [Exp]
type FunCall = (Id,[ActArgs])

data Exp = Id | Exp Op2 Exp | Op1 Exp | Int | Bool | FunCall | List | Tuple Exp Exp -- refers to the concrete value assigned to a 'Type'
data RetType = Type | Void
data Type = Id_ Id | Int_ Int | Bool_ Bool | Tuple_ Type Type | List_ Type -- refers to how you name your type in your language

-- Stmt is not a type !
-- FunDecl is not a type !

data Op1 = Negate | UnitaryMinus
data Op2 = Add | Sub | Mult | Div | Mod | Equals | Less | More | LessEq | MoreEq | NotEq | And | Or | AppCons
