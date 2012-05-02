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
		conv = [(PLUS, "add"), (MINUS, "sub"), (AND, "and"), (OR, "or"), (MUL, "mul"), (DIV, "div"), (MOD, "mod"), (XOR, "xor")]
		
	assemble (CALL (TEMP "alloc") args) = concatMap assemble args ++ ["stmh " ++ show (length args), "ldc " ++ show (length args - 1), "sub"]
	assemble (CALL (TEMP "head") args) = assemble (head args) ++ ["ldh 0"] -- get value
	assemble (CALL (TEMP "tail") args) = assemble (head args) ++ ["ldh 1"]
	assemble (CALL (TEMP "empty") args) = assemble (head args) ++ ["ldh 1", "ldc 0", "eq"]
	assemble (CALL (TEMP "print") args) = assemble (head args)++ ["trap 0"] -- hardcoded print function, TODO: add ajs?
	assemble (CALL (TEMP id) args) = concat (map assemble args) ++ ["ldc " ++ id, "jsr", "ajs -" ++ show (length args), "ldr RR"]
	
instance Assemble Stm where
	-- FIXME: can only move local vars
	assemble (MOVE (MEM (TEMP "_res")) e) = assemble e ++ ["str RR"]
--	assemble (MOVE (MEM (CONST v)) (CALL _ _)) = ["ldr RR"]
	assemble (MOVE (MEM (CONST v)) b) = assemble b ++ ["stl " ++ show v]
	assemble (EXP e) = assemble e
	assemble (LABEL l) = [l ++ ":"]
	assemble (SEQ a b) = assemble a ++ assemble b
	assemble (JUMP (NAME l) _) = ["bra " ++ l] -- FIXME: correct?
	-- corrects for consumption of the brf
	assemble (CJUMP o a b t f) = assemble a ++ assemble b ++ [op o] ++ ["brf " ++ f, "ldc 1", "brt " ++ t]
		where
		op o = case lookup o rel of
			Just c -> c 
		rel = [(EQ, "eq"), (LT, "lt"), (GT, "gt"), (LE, "le"), (GE, "ge"), (NE, "ne")]
	
instance Assemble Frame where
	assemble (Frame {curPos=c, IR.id=i, varMap=v, body=b}) = [i ++ ": "] ++ ["ldr MP"] ++ ["ldrr MP SP"] ++ ["ajs " ++ show c] ++ assemble b ++ ["ldrr SP MP"] ++ ["str MP"] ++ lastOp
		where
		lastOp = if i == "main" then ["halt"] else ["ret"]
	
instance Assemble Reg where
	assemble (Reg { frameList=f}) = ["bra main"] ++ concat (map assemble (Map.elems f))

instance Assemble IR where
	assemble (Ex e) = assemble e
	assemble (Nx n) = assemble n
	assemble (Cx f) = error "Cannot convert conditional"
