module SSM where

import Prelude hiding (seq, EQ, LT, GT)
import IR
import Data.Map as Map hiding (lookup, map)

class Assemble a where
	assemble :: a -> [String]

instance Assemble Exp where
	assemble (CONST a) = ["ldc " ++ show a]
	assemble (NAME s) = [s] -- FIXME
	assemble (TEMP l) = [l] -- FIXME
	assemble (MEM (CONST a)) = ["ldl " ++ show a]  -- FIXME: do something else?
	assemble (BINOP o a b) = assemble a ++ assemble b ++ [op o]
		where
		op o = case lookup o conv of
			Just c -> c 
		conv = [(PLUS, "add"), (MINUS, "sub"), (AND, "and"), (OR, "or"), (MUL, "mul"), (DIV, "div"), (MOD, "mod")]
		
	assemble (CALL (TEMP "print") args) = assemble (head args )++ ["trap 0"] -- hardcoded print function
	assemble (CALL (TEMP id) args) = concat (map assemble args) ++ ["bsr " ++ id] ++ ["ldr RR"]
	
instance Assemble Stm where
	-- FIXME: can only move local vars
	assemble (MOVE (MEM (TEMP "_res")) e) = assemble e ++ ["str RR"]
--	assemble (MOVE (MEM (CONST v)) (CALL _ _)) = ["ldr RR"]
	assemble (MOVE (MEM (CONST v)) b) = assemble b ++ ["stl " ++ show v]
	assemble (EXP e) = assemble e
	assemble (LABEL l) = [l ++ ":"]
	assemble (SEQ a b) = assemble a ++ assemble b
	assemble (JUMP (NAME l) _) = ["bra " ++ l] -- FIXME: correct?
	assemble (CJUMP o a b t f) = assemble a ++ assemble b ++ [op o] ++ ["brf " ++ f] ++ ["brt " ++ t]
		where
		op o = case lookup o rel of
			Just c -> c 
		rel = [(EQ, "eq"), (LT, "lt"), (GT, "gt"), (LE, "le"), (GE, "ge"), (NE, "ne")]
	
instance Assemble Frame where
	assemble (Frame {curPos=c, IR.id=i, varMap=v, body=b}) = assemble b ++ ["ret"]
	
instance Assemble Reg where
	assemble (Reg { frameList=f}) = concat (map assemble (Map.elems f))

instance Assemble IR where
	assemble (Ex e) = assemble e
	assemble (Nx n) = assemble n
	assemble (Cx f) = error "Cannot convert conditional"
