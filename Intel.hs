{-- Linux Intel machine assembly, assumes word length is 4 --}
module Intel where

import Prelude hiding (seq, EQ, LT, GT)
import IR
import Data.Map as Map hiding (lookup, map)

class Assemble a where
	assemble :: a -> [String]

instance Assemble Exp where
	assemble (CONST a) = ["ldc " ++ show a]
	assemble (NAME s) = [s]
	assemble (TEMP l) = [l]
	assemble (MEM (BINOP PLUS (TEMP "_glob") (CONST x))) = ["ldr 4", "lda " ++ show x]  
	assemble (MEM (CONST a)) = ["ldl " ++ show a]  -- FIXME: do something else?
	assemble (BINOP o a b) = assemble a ++ assemble b ++ [op o]
		where
		op o = case lookup o conv of
			Just c -> c 
		conv = [(PLUS, "add"), (MINUS, "sub"), (AND, "and"), (OR, "or"), (MUL, "mul"), (DIV, "div"), (MOD, "mod"), (XOR, "xor")]
		
	assemble (CALL (TEMP "_alloc") args) = concatMap assemble args ++ ["stmh " ++ show (length args), "ldc " ++ show (length args - 1), "sub"]
	assemble (CALL (TEMP "head") args) = assemble (head args) ++ ["ldh 0"]
	assemble (CALL (TEMP "tail") args) = assemble (head args) ++ ["ldh 1"]
	assemble (CALL (TEMP "fst") args) = assemble (head args) ++ ["ldh 0"]
	assemble (CALL (TEMP "snd") args) = assemble (head args) ++ ["ldh 1"]
	assemble (CALL (TEMP "empty") args) = assemble (head args) ++ ["ldh 1", "ldc 0", "eq"]
	assemble (CALL (TEMP "print") args) = assemble (head args)++ ["trap 0"]
	assemble (CALL (TEMP id) args) = concat (map assemble args) ++ ["ldc " ++ id, "jsr", "ajs -" ++ show (length args), "ldr RR"]
	
instance Assemble Stm where
	assemble (MOVE (MEM (BINOP PLUS (TEMP "_glob") (CONST x))) e) = ["ldr 4"] ++ assemble e ++ ["sta " ++ show x]
	assemble (MOVE (MEM (TEMP "_res")) e) = assemble e ++ ["str RR"]
	assemble (MOVE (MEM (CONST v)) b) = assemble b ++ ["stl " ++ show v]
	assemble (EXP e) = assemble e
	assemble (LABEL l) = [l ++ ":"]
	assemble (SEQ a b) = assemble a ++ assemble b
	assemble (JUMP (NAME l) _) = ["bra " ++ l]
	assemble (CJUMP o a b t f) = assemble b ++ assemble a ++ ["pop aex", "cmp aex,[esp]", "sub esp, 4", op o ++ f, "jump " ++ t]
		where
		op o = case lookup o rel of
			Just c -> c 
		rel = [(EQ, "je"), (LT, "jl"), (GT, "jg"), (LE, "jle"), (GE, "jge"), (NE, "jne")]

instance Assemble Global where
	assemble (Global {curVarPos=p,globVarMap=m,varBody=bm}) = ["ajs " ++ show p] ++ concat (map (\(i,e) -> assemble e ++ ["stl " ++ show i]) getList)
		where
		getList = Map.elems (Map.intersectionWith (\x y -> (x, y)) m bm)
	
-- FIXME add or sub?
instance Assemble Frame where
	assemble (Frame {curPos=c, IR.id=i, varMap=v, body=b}) = [i ++ ": "] ++ ["push ebp", "mov ebp,esp", "add esp, " ++ show 4*c] ++ assemble b ++ ["mov esp,ebp"] ++ ["pop ebp"] ++ lastOp
		where
		lastOp = if i == "main" then ["	mov	ebx,0", "mov eax,1", "int 0x80"] else ["ret"]
	
instance Assemble Reg where
	assemble (Reg { frameList=f, globVars=g}) = ["mov esi,ebp"] ++ assemble g ++  ["jump main"] ++ concat (map assemble (Map.elems f))

instance Assemble IR where
	assemble (Ex e) = assemble e
	assemble (Nx n) = assemble n
	assemble (Cx f) = error "Cannot convert conditional"
