module Parser where

import TreeLib

type Id = String

data Prog = Decl
data Decl = VarDecl | FunDecl

-- data VarDecl
-- data FunDecl

-- data FArgs
-- data ActArgs
-- data FunCall

data Exp = Exp Op2 Exp | Op1 Exp | FunCall | RetType -- is ret type ok in this case otherwise multiple declaration of the same Type?
data RetType = Type | Void
data Type = Num Int | Bool | Tuple Type Type | List Type | Id

-- data Stmt

data Op1 = Negate | UnitaryMinus
data Op2 = Add | Sub | Mult | Div | Mod | Equals | Less | More | LessEq | MoreEq | Not | And | Or -- what's ':'? -> I guess list = [x:xs]

t1 = Node 10 Tip Tip
t2 = Node 17 (Node 12 (Node 5 Tip (leaf 8)) (leaf 15))
              (Node 115
                    (Node 32 (leaf 30) (Node 46 Tip (leaf 57)))
                    (leaf 163))
main = do pict t2

-- probably need for conversion functions that pattern match on arguments like conv (x,y) = Tuple x y
-- in the end should declare Prog =... and main = do Prog