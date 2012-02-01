module Grammar where

type Id = String

data Prog = Decl
data Decl = VarDecl | FunDecl

-- data VarDecl
-- data FunDecl

 -- use tuple of Type and Id as arguments?
 -- bit strange that Type can also be Id
type FArgs = [(Type, Id)]
-- data ActArgs
-- data FunCall

data Exp = Exp Op2 Exp | Op1 Exp | FunCall | RetType -- is ret type ok in this case otherwise multiple declaration of the same Type?
data RetType = Type | Void
data Type = Num Int | Bool | Tuple Type Type | List Type | Id

-- data Stmt

data Op1 = Negate | UnitaryMinus
data Op2 = Add | Sub | Mult | Div | Mod | Equals | Less | More | LessEq | MoreEq | Not | And | Or | Concat 
