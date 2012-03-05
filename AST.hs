module AST where

type Id = String

type Prog = [Decl]
data Decl = VarDecl VarDecl | FunDecl FunDecl

data VarDecl = VD Type Id Exp
data FunDecl = FD RetType Id FArgs [VarDecl] Stmt
type FArgs = [(Type, Id)]
type ActArgs = [Exp]
type FunCall = (Id, ActArgs)

data Exp = ExpOp_ ExpOp Exp Exp | Int Int | Id Id  | Op1_ Op1 Exp | Bool Bool | FunCall FunCall | EmptyList | Tuple Exp Exp
data RetType = Type Type | Void
data Type = Id_ Id | Int_ | Bool_ | Tuple_ Type Type | List_ Type
data Stmt = Seq [Stmt] | If Exp Stmt | IfElse Exp Stmt Stmt | While Exp Stmt | Assign Id Exp | FunCall_ FunCall | Return Exp
data Op1 = Negate | UnitaryMinus
data ExpOp = Add | Sub | Mod | Equals | Less | More | LessEq | MoreEq | NotEq | And | Or | AppCons | Mul | Div deriving Eq

instance Show Type where
	show (Id_ x) = x
	show Int_ = "int"
	show Bool_ = "bool"
	show (Tuple_ x y) = "(" ++ show x ++ "," ++ show y ++ ")"
	show (List_ x) = "[" ++ show x ++ "]"
	
instance Show RetType where
    show Void = "void"
    show (Type t) = show t

instance Show Decl where
    show (VarDecl varDecl) = show varDecl
    show (FunDecl funDecl) = show funDecl

instance Show VarDecl where
    show (VD varType ident ass) = show varType ++ " " ++ ident ++ " = " ++ show ass ++ ";"
	
instance Show FunDecl where
	show (FD retType ident fArgs varDecl stmt) = "\n" ++ show retType ++ " " ++ ident ++ "(" ++ showArgs ++ ")" ++ "\n{\n" ++ showDecl ++ showStmt ++ "}"
	 where
	 showArgs = addsep ", " (map (\(t, i) -> show t ++ " " ++ i) fArgs) 
	 showDecl = foldr (\x y -> x ++ "\n" ++ y) "" (map (indent . show) varDecl)
	 showStmt = indentML (show stmt)
	
instance Show Stmt where 
    show (Seq stmt) = unlines $ map show stmt
    show (If exp stmt) = "if(" ++ show exp ++ ")\n{\n" ++ indentML(show stmt) ++ "}"
    show (IfElse exp stmt1 stmt2) = "if(" ++ show exp ++ ")" ++ "\n{\n" ++ indentML (show stmt1) ++ "}\nelse\n{\n" ++ indentML (show stmt2) ++ "}"
    show (While exp stmt) = "while(" ++ show exp ++ ")\n{\n" ++ indentML (show stmt) ++ "}"
    show (Assign ident exp) = ident ++ " = " ++ show exp ++ ";"
    show (FunCall_ (id, args)) = id ++ "(" ++ addsep ", " (map show args) ++ ");"
    show (Return exp) = "return " ++ show exp ++ ";"

-- shows ExpOp_ and filters parentheses
showOp2 (ExpOp_ o e1 e2)
   | elem o op7 = par (op6 ++ op5 ++ op4 ++ op3 ++ op2) e1 ++ " " ++ show o ++ " " ++ par (op7 ++ op6 ++ op5 ++ op4 ++ op3 ++ op2) e2
   | elem o op6 = par (op5 ++ op4 ++ op3 ++ op2) e1 ++ " " ++ show o ++ " " ++ par (op6 ++ op5 ++ op4 ++ op3 ++ op2) e2 
   | elem o op5 = par (op5 ++ op4 ++ op3 ++ op2) e1 ++ " " ++ show o ++ " " ++ par (op4 ++ op3 ++ op2) e2
   | elem o op4 = par (op3 ++ op2) e1 ++ " " ++ show o ++ " " ++ par (op3 ++ op2) e2
   | elem o op3 = par (op3 ++ op2) e1 ++ " " ++ show o ++ " " ++ par (op2) e2
   | elem o op2 = par (op2) e1 ++ " " ++ show o ++ " " ++ show e2
   where
   par list x@(ExpOp_ o2 e3 e4) = if (elem o2 list) then "(" ++ show x ++ ")" else show x
   par list x = show x
   op7 = [Mul, Div, Mod]
   op6 = [Add, Sub]
   op5 = [AppCons]
   op4 = [Equals, LessEq, MoreEq, NotEq, Less, More]
   op3 = [And]
   op2 = [Or]
 
instance Show Exp where
   show k@(ExpOp_ o e1 e2) = showOp2 k
   show (Int int) = show int
   show (Id id) = id
   show (Op1_ op e) = show op++ "(" ++ show e ++ ")"
   show (Bool bool) = if (bool) then "true" else "false"
   show (FunCall (id, args)) = id ++ "(" ++ addsep ", " (map show args) ++ ")"
   show EmptyList = "[]"
   show (Tuple e1 e2) = "(" ++ show e1 ++ "," ++ show e2 ++ ")"
   
instance Show ExpOp where
   show Add = "+"
   show Sub = "-"
   show Mod = "%"
   show Equals = "=="
   show Less = "<"
   show More = ">"
   show LessEq = "<="
   show MoreEq = ">="
   show NotEq = "!="
   show And = "&&"
   show Or = "||"
   show AppCons = ":"
   show Mul = "*"
   show Div = "/"
   
instance Show Op1 where
   show UnitaryMinus = "-"
   show Negate = "!"
   
indentML x = unlines $ map indent (lines $ x)
indent x = "\t" ++ x;    
 
addsep  :: String -> [String] -> String
addsep _ []            =  ""
addsep sep ws          =  foldr1 (\w s -> w ++ sep ++ s) ws

prettyPrint :: Prog -> [String]
prettyPrint x = map show x
