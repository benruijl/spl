{-- Linux Intel machine assembly, assumes word length is 4 --}
-- TODO: make a working Hello World!
-- FIXME: functions with empty body not allowed
module Intel where

import Prelude hiding (seq, EQ, LT, GT)
import IR
import Data.Map as Map hiding (lookup, map)

class Assemble a where
	assemble :: a -> [String]

instance Assemble Exp where
	assemble (CONST a) = ["mov eax, " ++ show a, "push eax"]
	assemble (NAME s) = [s]
	assemble (TEMP l) = [l]
	assemble (MEM (BINOP PLUS (TEMP "_glob") (CONST x))) = ["push dword[esi - " ++ show (4*x) ++ "]"]  
	assemble (MEM (CONST a)) = ["push dword[ebp -" ++ show (4* a) ++ "]"]
	-- TODO: fix modulo
	assemble (BINOP o a b) = assemble a ++ assemble b ++ ["pop ebx", "pop eax", op o ++ " eax, ebx", "push eax"]
		where
		op o = case lookup o conv of
			Just c -> c 
		conv = [(PLUS, "add"), (MINUS, "sub"), (AND, "and"), (OR, "or"), (MUL, "imul"), (DIV, "div"), (MOD, "mod"), (XOR, "xor")]
		
	assemble (CALL (TEMP "_alloc") args) = ["push dword " ++ show (4 * length args), "call malloc", "add esp, 4", "push eax"] ++ concatMap (\(i,x) -> assemble x ++ ["pop eax", "pop ebx", "mov [ebx +" ++ show (i * 4) ++ "], eax", "push ebx"]) (zip [0..] args)
	assemble (CALL (TEMP "head") args) = assemble (head args) ++ ["pop eax", "push dword[eax]"]
	assemble (CALL (TEMP "tail") args) = assemble (head args) ++ ["ldh 1"]
	assemble (CALL (TEMP "fst") args) = assemble (head args) ++ ["ldh 0"]
	assemble (CALL (TEMP "snd") args) = assemble (head args) ++ ["ldh 1"]
	assemble (CALL (TEMP "empty") args) = assemble (head args) ++ ["ldh 1", "ldc 0", "eq"]
	-- FIXME: only prints single digit
	assemble (CALL (TEMP "print") args) = assemble (head args) ++ ["pop eax", "add eax,30h", "push eax", "mov eax,4", "mov ecx,esp", "mov ebx,1", "mov edx,1", "int 0x80", "add esp, 4"]
	assemble (CALL (TEMP id) args) = concat (map assemble args) ++ ["call " ++ id, "add esp, " ++ show (4 * length args), "push eax"] -- FIXME: what happens if there is no return value?
	
instance Assemble Stm where
	assemble (MOVE (MEM (BINOP PLUS (TEMP "_glob") (CONST x))) e) = ["ldr 4"] ++ assemble e ++ ["sta " ++ show x]
	assemble (MOVE (MEM (TEMP "_res")) e) = assemble e ++ ["pop eax"]-- store in EAX
	assemble (MOVE (MEM (CONST v)) b) = assemble b ++ ["pop eax", "mov [ebp - " ++ show (4*v) ++ "], eax"]
	assemble (EXP e) = assemble e
	assemble (LABEL l) = [l ++ ":"]
	assemble (SEQ a b) = assemble a ++ assemble b
	assemble (JUMP (NAME l) _) = ["jmp " ++ l]
	assemble (CJUMP o a b t f) = assemble b ++ assemble a ++ ["pop eax", "pop ebx", "cmp eax,ebx", op o ++ " " ++ t, "jmp " ++ f]
		where
		op o = case lookup o rel of
			Just c -> c 
		rel = [(EQ, "je"), (LT, "jl"), (GT, "jg"), (LE, "jle"), (GE, "jge"), (NE, "jne")]

instance Assemble Global where
	assemble (Global {curVarPos=p,globVarMap=m,varBody=bm}) = ["sub esp, " ++ show (4 *p)] ++ concat (map (\(i,e) -> assemble e ++ ["mov [ebp - " ++ show (4*i) ++ "]"]) getList)
		where
		getList = Map.elems (Map.intersectionWith (\x y -> (x, y)) m bm)
	
-- FIXME add or sub?
instance Assemble Frame where
	assemble (Frame {curPos=c, IR.id=i, varMap=v, body=b}) = [i ++ ": "] ++ ["push ebp", "mov ebp,esp", "sub esp, " ++ show (4*c)] ++ assemble b ++ ["mov esp,ebp"] ++ ["pop ebp"] ++ lastOp
		where
		lastOp = if i == "main" then ["mov ebx,0", "mov eax,1", "int 0x80"] else ["ret"]
	
instance Assemble Reg where
	assemble (Reg { frameList=f, globVars=g}) = ["extern malloc", "section .text", "global main", "mov esi,ebp"] ++ assemble g ++  ["jmp main"] ++ concat (map assemble (Map.elems f))

instance Assemble IR where
	assemble (Ex e) = assemble e
	assemble (Nx n) = assemble n
	assemble (Cx f) = error "Cannot convert conditional"
