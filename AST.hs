module AST where

type Id = String

data Prog = Decl deriving (Show)
data Decl = VarDecl | FunDecl deriving (Show)

-- make a type?
data VarDecl = VD Type Id Exp deriving (Show)
data FunDecl = FD RetType Id FArgs [VarDecl] [Stmt] deriving (Show)

type FArgs = [(Type, Id)]
type ActArgs = [Exp]
type FunCall = (Id, ActArgs)

-- the data structure should consist of two Exps even when the grammer splits it up into Terms
data Exp = ExpOp_ ExpOp Exp Exp | Term_ Term | Op1 Exp | Bool Bool | FunCall FunCall | EmptyList | Tuple Exp Exp deriving (Show)

data Term = TermOp TermOp Term Factor | Factor Factor deriving (Show)
data Factor = Int Int | Id Id | Exp_ Exp deriving (Show)


data RetType = Type Type | Void deriving (Show)
data Type = Id_ | Int_ | Bool_ | Tuple_ Type Type | List_ Type  deriving (Show)

data Stmt = List [Stmt] | If Exp Stmt | IfElse Exp Stmt Stmt | While Exp Stmt | Assign Id Exp | FunCall_ FunCall | Return Exp deriving (Show)


data Op1 = Negate | UnitaryMinus deriving (Show)

-- TODO: find out which belongs where
data ExpOp = Add | Sub | Mod | Equals | Less | More | LessEq | MoreEq | NotEq | And | Or | AppCons deriving (Show)
data TermOp =  Mult | Div deriving (Show)

{-
Order of operations:

infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -
infixr 5  :
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`
-}
