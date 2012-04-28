module SSM where

import Prelude hiding (seq, EQ, LT, GT)
import IR

class Assemble a where
	assemble :: a -> [String]

instance Assemble Exp where
	assemble (CONST a) = ["ldc " ++ show a]
	assemble (NAME s) = [s]
	assemble (TEMP l) = [l]
	assemble (MEM e) = assemble e ++ [] -- FIXME: do something else?
	assemble (BINOP o a b) = assemble a ++ assemble b ++ [op o]
		where
		op o = case lookup o conv of
			Just c -> c 
		conv = [(PLUS, "add"), (MINUS, "sub"), (AND, "and"), (OR, "or"), (MUL, "mul"), (DIV, "div"), (MOD, "mod")]
	
instance Assemble Stm where
	assemble (EXP e) = assemble e
	assemble (LABEL l) = [l ++ ":"]
	assemble (SEQ a b) = assemble a ++ assemble b
	assemble (CJUMP o a b t f) = assemble a ++ assemble b ++ [op o] ++ ["brf " ++ f] ++ ["brt " ++ t]
		where
		op o = case lookup o rel of
			Just c -> c 
		rel = [(EQ, "eq"), (LT, "lt"), (GT, "gt"), (LE, "le"), (GE, "ge"), (NE, "ne")]

instance Assemble IR where
	assemble (Ex e) = assemble e
	assemble (Nx n) = assemble n
	assemble (Cx f) = error "Cannot convert conditional"
