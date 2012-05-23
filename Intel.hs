{-- Linux Intel machine assembly, assumes word length is 4 --}
-- FIXME: functions with empty body not allowed
module Intel where

import Prelude hiding (seq, EQ, LT, GT)
import IR
import Data.Map as Map hiding (lookup, map)

class Assemble a where
	assemble :: a -> [String]

instance Assemble Exp where
	assemble (CONST a) = ["mov eax, " ++ show a]
	assemble (NAME s) = [s]
	assemble (TEMP l) = [l]
	assemble (MEM (BINOP PLUS (TEMP "_glob") (CONST x))) = ["mov eax, dword[esi - " ++ show (4*x) ++ "]"]  
	assemble (MEM (CONST a)) = ["mov eax, dword[ebp -" ++ show (4* a) ++ "]"]
	assemble (BINOP DIV a b) = assemble b ++ ["push eax"] ++ assemble a ++ ["pop ebx", "mov edx, 0", "idiv ebx"]
	assemble (BINOP MOD a b) = assemble b ++ ["push eax"] ++ assemble a  ++ ["pop ebx", "mov edx, 0", "idiv ebx", "mov eax, edx"]
	assemble (BINOP o a b) = assemble b ++ ["push eax"] ++ assemble a ++ ["pop ebx", op o ++ " eax, ebx"]
		where
		op o = case lookup o conv of
			Just c -> c 
		conv = [(PLUS, "add"), (MINUS, "sub"), (AND, "and"), (OR, "or"), (MUL, "imul"), (XOR, "xor")]
		
	assemble (CALL (TEMP "_alloc") args) = ["push dword " ++ show (4 * length args), "call malloc", "add esp, 4", "push eax"] ++ concatMap (\(i,x) -> assemble x ++ ["pop ebx", "mov [ebx +" ++ show (i * 4) ++ "], eax", "push ebx"]) (zip [0..] args)
	assemble (CALL (TEMP "head") args) = assemble (head args) ++ ["mov eax, dword[eax]"]
	assemble (CALL (TEMP "tail") args) = assemble (head args) ++ ["mov eax, dword[eax + 4]"]
	assemble (CALL (TEMP "fst") args) = assemble (head args) ++ ["mov eax, dword[eax]"]
	assemble (CALL (TEMP "snd") args) = assemble (head args) ++ ["mov eax, dword[eax + 4]"]
	assemble (CALL (TEMP "empty") args) = assemble (head args) ++ ["mov eax, dword[eax + 4]", "xor eax, 1"]
	assemble (CALL (TEMP "print") args) = assemble (head args) ++ ["push eax", "push dword print_int", "call printf", "add esp, 8"]
	assemble (CALL (TEMP "printChar") args) = assemble (head args) ++ ["push eax", "push dword print_char", "call printf", "add esp, 8"]
	assemble (CALL (TEMP id) args) = concat (map (\x -> assemble x ++ ["push eax"]) args) ++ ["call " ++ id, "add esp, " ++ show (4 * length args)]
	
instance Assemble Stm where
	assemble (MOVE (MEM (BINOP PLUS (TEMP "_glob") (CONST x))) e) = assemble e ++ ["mov [esi - " ++ show x ++ "], eax"]
	assemble (MOVE (MEM (TEMP "_res")) e) = assemble e -- stored in eax
	assemble (MOVE (MEM (CONST v)) b) = assemble b ++ ["mov [ebp - " ++ show (4*v) ++ "], eax"]
	assemble (EXP e) = assemble e
	assemble (LABEL l) = [l ++ ":"]
	assemble (SEQ a b) = assemble a ++ assemble b
	assemble (JUMP (NAME l) _) = ["jmp " ++ l]
	assemble (CJUMP o a b t f) = assemble b ++ ["push eax"] ++ assemble a ++ ["pop ebx", "cmp eax,ebx", op o ++ " " ++ t, "jmp " ++ f]
		where
		op o = case lookup o rel of
			Just c -> c 
		rel = [(EQ, "je"), (LT, "jl"), (GT, "jg"), (LE, "jle"), (GE, "jge"), (NE, "jne")]

instance Assemble Global where
	assemble (Global {curVarPos=p,globVarMap=m,varBody=bm}) = ["sub esp, " ++ show (4 *p)] ++ concat (map (\(i,e) -> assemble e ++ ["mov [ebp - " ++ show (4*i) ++ "],  eax"]) getList)
		where
		getList = Map.elems (Map.intersectionWith (\x y -> (x, y)) m bm)
	
-- FIXME add or sub?
instance Assemble Frame where
	assemble (Frame {curPos=c, IR.id=i, varMap=v, body=b}) = [i ++ ": "] ++ ["push ebp", "mov ebp,esp", "sub esp, " ++ show (4*c)] ++ assemble b ++ ["mov esp,ebp"] ++ ["pop ebp"] ++ lastOp
		where
		lastOp = if i == "main" then ["push 0", "call exit"] else ["ret"]
	
instance Assemble Reg where
	assemble (Reg { frameList=f, globVars=g}) = ["extern malloc,printf,exit", "segment .data", "GLOB dd " ++ show (getGlobSize g), "print_int db \"%d\", 10, 0", "print_char db \"%c\", 0", "section .text", "global main", "mov esi,ebp"] ++ assemble g ++  ["jmp main"] ++ concat (map assemble (Map.elems f))
		where
		getGlobSize (Global {curVarPos=p}) = 4 *p

instance Assemble IR where
	assemble (Ex e) = assemble e
	assemble (Nx n) = assemble n
	assemble (Cx f) = error "Cannot convert conditional"
