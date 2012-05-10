module Parser where

import AST
import Scanner
import MaybeCombinators

type Parser a = CoreScanner a [Token]

{-
Order of operations:
infixr 8  ^, ^^, **
infixl 7  *, /, %
infixl 6  +, -
infixr 5  :
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
-}

opAdd =  (match "+") >-> (\_ -> Add)
opSub =  (match "-") >-> (\_ -> Sub)
opMult =  (match "*") >-> (\_ -> Mul)
opDiv  =  (match "/") >-> (\_ -> Div)
opMod =  (match "%") >-> (\_ -> Mod)
opEquals =  (match "==") >-> (\_ -> Equals)
opLess =  (match "<") >-> (\_ -> Less)
opMore =  (match ">") >-> (\_ -> More)
opLessEq =  (match "<=") >-> (\_ -> LessEq)
opMoreEq =  (match ">=") >-> (\_ -> MoreEq)
opNotEq =  (match "!=") >-> (\_ -> NotEq)
opAnd =  (match "&&") >-> (\_ -> And)
opOr =  (match "||") >-> (\_ -> Or)
opAppCons =  (match ":") >-> (\_ -> AppCons)
opNegate =  (match "!") >-> (\_ -> Negate)
opUnitaryMinus =  (match "-") >-> (\_ -> UnitaryMinus)

op1 = opNegate ! opUnitaryMinus
op4 = opEquals ! opLessEq ! opMoreEq ! opNotEq ! opLess ! opMore
op6 = opAdd ! opSub
op7 = opMult ! opDiv ! opMod

-- TODO: improve
idScan = next ? (\x -> case x of {(Id__ a) -> True; (_) -> False;}) >-> (\(Id__ a) -> a)
intScan = next ? (\x -> case x of {(Int__ a) -> True; (_) -> False;}) >-> (\(Int__ a) -> Int a)
match b = next ? (\x -> case x of {(String_ a) -> a == b; (Id__ a) -> a == b; (_) -> False;}) -- match a string or id

progParse :: Parser Prog
progParse = iter (funDeclParse >-> (\x -> FunDecl x) ! varDeclParse >-> (\x -> VarDecl x))

fArgsParse :: Parser FArgs
fArgsParse = (( typeParse # idScan) >-> (\x -> [x])) /?\ (\x -> (match ",") >>| fArgsParse >-> (\y -> x ++ y))

-- accepts empty functions. add check here or check later?
funDeclParse :: Parser FunDecl
funDeclParse = ( retTypeParse # idScan >>- (match "(") # (fArgsParse ! tuple []) >>- (match ")") >>- (match "{") # (iter varDeclParse) # (iter stmtParse) >>- (match "}")) >-> (\((((t,i),a),v),s) -> FD t i a v (Seq s))

funCallParse :: Parser FunCall
funCallParse = idScan >>- (match "(") # (actArgsParse ! tuple []) >>- (match ")")

actArgsParse :: Parser ActArgs
actArgsParse = (expParse >-> (\x -> [x])) /?\ (\x -> (match ",") >>| actArgsParse >-> (\y -> x ++ y))

-- allow for void types
retTypeParse :: Parser Type
retTypeParse = (match "void") >-> (\x -> Void) ! typeParse

-- note: each variable has to be initialised!
varDeclParse :: Parser VarDecl
varDeclParse = ( typeParse # idScan >>- (match "=") # expParse >>- (match ";")) >-> (\((x, y),z) -> VD x y z)

-- Right recursive expression parser
nextr :: Parser ExpOp -> Parser Exp -> Parser Exp
nextr o p = p /?\ next'
	where
	next' x = o # (p /?\ next')  >-> (\(o,y) -> ExpOp_ o x y)
	
-- Left recursive expression parser
nextl :: Parser ExpOp -> Parser Exp -> Parser Exp
nextl o p = p /?\ next'
	where
	next' x = (o # p >-> (\(o,y) -> ExpOp_ o x y))  /?\ next'
	
expParse :: Parser Exp
expParse = nextr opOr andParse
    where
    andParse = nextr opAnd compParse
    compParse = nextr op4 listAddParse
    listAddParse = nextr opAppCons addSubParse
    addSubParse = nextl op6 termParse
    termParse = nextl op7 factorParse

factorParse :: Parser Exp
factorParse = fcParse ! boolParse ! tupleParse ! op1Parse ! (idScan >-> (\x -> Id x)) ! intScan ! emptyListParse ! (match "(" >>| expParse >>- match ")")
  where
  emptyListParse = match "[" >>- match "]" >-> (\_ -> EmptyList)
  tupleParse = match "(" >>| expParse >>- match "," # expParse >>- match ")" >-> (\(x,y) -> Tuple x y)
  fcParse = funCallParse >-> (\x -> FunCall x)
  boolParse = (match "true") >-> (\x -> Bool True) ! (match "false") >-> (\x -> Bool False)
  op1Parse = op1 # factorParse >-> (\(o, x) -> Op1_ o x) -- only accepts factors
    
stmtParse = returnParse ! ifElseParse ! (funCallParse  >>- (match ";") >-> (\x -> FunCall_ x)) ! curlyParse ! assignParse ! whileParse

curlyParse = match "{" >>| iter stmtParse >>- match "}" >-> (\x -> Seq x)

ifElseParse :: Parser Stmt
ifElseParse = ((match "if") >>| (match "(") >>| expParse  >>- (match ")") # stmtParse >-> (\(e,s) -> (If e s))) /?\ (\(If e s) -> (match "else") >>| stmtParse >-> (\c -> (IfElse e s c)))

assignParse :: Parser Stmt
assignParse = idScan >>- (match "=") # expParse >>- parseEnd >-> (\(x,y) -> Assign x y)

whileParse = (match "while") >>| (match "(") >>| expParse  >>- (match ")") # stmtParse >-> (\(x,y) -> While x y)

returnParse =  (match "return") >>| ((expParse >>- parseEnd >-> (\x -> Return (Just x))) ! (parseEnd >-> (\x -> Return Nothing)))

typeParse = (match "int") >-> (\_ -> Int_) ! (match "bool") >-> (\_ -> Bool_) ! parseTuple ! parseList ! idScan >-> (\x -> Generic_ x)
    where
    parseTuple = match "(" >>| typeParse >>- match "," # typeParse >>- match ")" >-> (\(x,y) -> Tuple_ x y)
    parseList = match "[" >>| typeParse >>- match "]" >-> (\x -> List_ x)

parseEnd = match ";"

