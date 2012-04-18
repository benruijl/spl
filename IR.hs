module IR where

import Prelude (String, Int, Bool(..), Maybe(..), error, const, lookup, ($))
import qualified AST
import Typing

type Label = String
data BinOp = PLUS | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
data RelOp = EQ | NE| LT | GT| LE | GE | ULT | ULE | UGT | UGE
data Exp = CONST Int | NAME String | TEMP Label | BINOP BinOp Exp Exp | MEM Exp | CALL Exp [Exp] | ESEQ Stm Exp
-- TODO: what to do with the [Exp] in JUMP?
data Stm = MOVE Exp Exp | EXP Exp | JUMP Exp [Exp] | CJUMP RelOp Exp Exp Label Label | SEQ Stm Stm | LABEL String
data IR = Ex Exp | Nx Stm | Cx (Label -> Label -> Stm)

unEx :: IR -> Exp
unNx :: IR -> Stm
unCx :: IR -> (Label -> Label -> Stm)

-- make a sequence from list
seq :: [Stm] -> Stm
seq [x] = x
seq (x:xs) = SEQ x (seq xs)

-- TODO: make labels unique
-- TODO: add special case for CONST 1, CONST 0 for Cx?
--unCx (Nx s) should not happen
unEx (Ex e) = e
unEx (Nx s) = ESEQ s (CONST 0)
unEx (Cx c) = ESEQ (seq [(MOVE (TEMP "r") (CONST 1)), (c "t" "f"), (LABEL "f"), (MOVE (TEMP "r") (CONST 1)), (LABEL "t")]) (TEMP "r")
unNx (Ex e) = EXP e
unNx (Nx s) = s
unNx (Cx c) = SEQ (c "a" "a") (LABEL "a")
unCx (Ex e) = CJUMP EQ e (CONST 1)
unCx (Cx c) = c

class Convert a where
	convert :: a -> Env -> IR

instance Convert AST.Exp where
	convert (AST.Int a)  =const $ Ex $ CONST a
	convert (AST.Bool False) = const $ Ex $ CONST 0
	convert (AST.Bool True) = const $ Ex $ CONST 1
	{-convert EmptyList =
	convert (ExpOp_ AppCons a EmptyList) = 
	convert (ExpOp_ AppCons a b) =
	convert (ExpOp_ o a b) = 
	convert (Op1_ UnitaryMinus a) =
	convert (Op1_ Negate a) =
	convert (Tuple a b) = 
	convert (Id name)-}

-- TODO: add negate, and, or
{-getCond (AST.Bool True) = (EQ, CONST 1, CONST 1)
getCond (AST.Bool False) = (EQ, CONST 1, CONST 0)
getCond (AST.ExpOp_ o x y) = (getOp o, convert x, convert y)
	where
	getOp = case lookup o m of
		Just a -> a
		Nothing -> error $ "Undefined operator"
	m = [(AST.Equals, EQ), (AST.Less, LT), (AST.More, GT), (AST.LessEq, LE), (AST.MoreEq, GE), (AST.NotEq, NE)]-}

convert2 ::  AST.Stmt -> Env -> IR
-- TODO: where to jump on false?
--convert2 (If cond stmt) = \((o, x, y) -> CJUMP o x y (convert stmt) ) (getCond cond)
--convert2 (IfElse cond stmt1 stmt2) = \((o, x, y) -> CJUMP o x y (convert stmt1) (convert stmt2)) (getCond cond)

-- TODO: unique labels
convert2 (AST.While cond stmt) = \e -> parseWhile (convert cond e) (convert2 stmt e) 
	where
	parseWhile c s = Nx (seq [LABEL "test", unCx c "body" "done", LABEL "body", unNx s, JUMP (NAME "test") [], LABEL "done"])
{-convert2 (Assign id exp) =
convert2 (While cond stmt) =
convert2 (Seq stmt) =
convert2 (FunCall_ funCall) =
convert2 (Return a) = -}

--convert3 :: AST.VarDecl -> Stm
--convert3 (VD t id exp) = 
