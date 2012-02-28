module AST where

type Id = String

type Prog = [Decl]
data Decl = VarDecl VarDecl | FunDecl FunDecl

data VarDecl = VD Type Id Exp
data FunDecl = FD RetType Id FArgs [VarDecl] Stmt
type FArgs = [(Type, Id)]
type ActArgs = [Exp]
type FunCall = (Id, ActArgs)

-- AST structure, not the same as the grammar structure
data Exp = ExpOp_ ExpOp Exp Exp | Int Int | Id Id  | Op1_ Op1 Exp | Bool Bool | FunCall FunCall | EmptyList | Tuple Exp Exp

data RetType = Type Type | Void
data Type = Id_ Id | Int_ | Bool_ | Tuple_ Type Type | List_ Type

data Stmt = Seq [Stmt] | If Exp Stmt | IfElse Exp Stmt Stmt | While Exp Stmt | Assign Id Exp | FunCall_ FunCall | Return Exp

-- instead of If and IfElse we could have had If Exp Stmt (Maybe Stmt) but let's leave it like this for now it works

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
    show (If exp stmt) = "If(" ++ show exp ++ ")\n{\n" ++ show stmt ++ "}"
    show (IfElse exp stmt1 stmt2) = "If(" ++ show exp ++ ")" ++ "\n{\n" ++ indentML (show stmt1) ++ "}\nelse\n{\n" ++ indentML (show stmt2) ++ "}"
    show (While exp stmt) = "While(" ++ show exp ++ ")\n{\n" ++ indentML (show stmt) ++ "}"
    show (Assign ident exp) = ident ++ " = " ++ show exp ++ ";"
    show (FunCall_ (id, args)) = id ++ "(" ++ addsep ", " (map show args) ++ ");"
    show (Return exp) = "return " ++ show exp ++ ";"
   
instance Show Exp where
   -- Only show parentheses when required. These are probably not all cases
   -- TODO: add parentheses for Or, And, AppCons
   show (ExpOp_ o e1 e2)
      | o == Mul || o == Div = parcheck e1 ++ " " ++ show o ++ " " ++ parcheck e2
      | o == Sub = show e1 ++ " " ++ show o ++ " " ++ parcheck e2 
   	  | otherwise =  show e1 ++ " " ++ show o ++ " " ++ show e2
   	  where
   	  parcheck s@(ExpOp_ o e1 e2) = if (o == Add || o == Sub) then "(" ++ show s ++ ")" else show s
   	  parcheck s = show s
   
   show (Int int) = show int
   show (Id id) = id
   show (Op1_ op e) = show op++ "(" ++ show e ++ ")"
   show (Bool bool) = show bool
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
