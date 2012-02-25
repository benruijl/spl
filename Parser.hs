module Parser where

import Scanner
import AST
import Char
import Combinators

type Parser a = Scanner a

{-
Order of operations:

infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -
infixr 5  :
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`

TODO: implement all of them, currently only 6 and 7 are done
-}

opAdd =  (matchChar '+') >-> (\_ -> Add)
opSub =  (matchChar '-') >-> (\_ -> Sub)
opMult =  (matchChar '*') >-> (\_ -> Mul)
opDiv  =  (matchChar '/') >-> (\_ -> Div)
opMod =  (matchChar '%') >-> (\_ -> Mod)
opEquals =  (matchChar '=') #  (matchChar '=') >-> (\_ -> Equals)
opLess =  (matchChar '<') >-> (\_ -> Less)
opMore =  (matchChar '>') >-> (\_ -> More)
opLessEq =  (matchChar '<') #  (matchChar '=') >-> (\_ -> LessEq)
opMoreEq =  (matchChar '>') #  (matchChar '=') >-> (\_ -> MoreEq)
opNotEq =  (matchChar '!') #  (matchChar '=') >-> (\_ -> NotEq)
opAnd =  (matchChar '&') #  (matchChar '&') >-> (\_ -> And)
opOr =  (matchChar '|') #  (matchChar '|') >-> (\_ -> Or)
opAppCons =  (matchChar ':') >-> (\_ -> AppCons)
opNegate =  (matchChar '!') >-> (\_ -> Negate)
opUnitaryMinus =  (matchChar '-') >-> (\_ -> UnitaryMinus)

op4 = opEquals ! opLessEq ! opMoreEq ! opNotEq ! opLess ! opMore
op6 = opAdd ! opSub
op7 = opMult ! opDiv ! opMod

op1 = opNegate ! opUnitaryMinus 

identScan = (alphaScan >-> (\x -> [x])) # (iter alphaNumUnderScoreScan) >-> cat1

intScan = ((((matchChar '-') >-> (\x->[x])) # (iter digitScan)) >-> cat1 ! (iter digitScan) ? (/="")) >-> (\x -> Int (toNum x))

progParse :: Parser Prog
progParse = iter (funDeclParse >-> (\x -> FunDecl x) ! varDeclParse >-> (\x -> VarDecl x)) -- ? (/=[]) it doesn't have eq but [] should be generally comparable

fArgsParse :: Parser FArgs
fArgsParse = ((token typeParse # identScan) >-> (\x -> [x])) /?\ (\x -> (matchChar ',') >>| fArgsParse >-> (\y -> x ++ y))

-- accepts empty functions. add check here or check later?
funDeclParse :: Parser FunDecl
funDeclParse = (token retTypeParse # identScan >>- (matchChar '(') # (fArgsParse ! tuple []) >>- (matchChar ')') >>- (matchChar '{') # (iter varDeclParse) # (iter stmtParse) >>- (matchChar '}')) >-> (\((((t,i),a),v),s) -> FD t i a v (Seq s))

funCallParse :: Parser FunCall
funCallParse = identScan >>- (matchChar '(') # (actArgsParse ! tuple []) >>- (matchChar ')')

actArgsParse :: Parser ActArgs
actArgsParse = (expParse >-> (\x -> [x])) /?\ (\x -> (matchChar ',') >>| actArgsParse >-> (\y -> x ++ y))

retTypeParse :: Parser RetType
retTypeParse = typeParse >-> (\x -> Type x) ! wordScan ? (=="void") >-> (\x -> Void)

-- note: each variable has to be initialised!
varDeclParse :: Parser VarDecl
varDeclParse = (token typeParse # identScan >>- (matchChar '=') # expParse >>- (matchChar ';')) >-> (\((x, y),z) -> VD x y z)

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
factorParse = fcParse ! boolParse ! tupleParse ! (identScan >-> (\x -> Id x)) ! intScan ! emptyListParse ! (matchChar '(' >>| expParse >>- matchChar ')')
  where
  emptyListParse = matchChar '[' >>- matchChar ']' >-> (\_ -> EmptyList)
  tupleParse = matchChar '(' >>| expParse >>- matchChar ',' # expParse >>- matchChar ')' >-> (\(x,y) -> Tuple x y)
  fcParse = funCallParse >-> (\x -> FunCall x)
  boolParse = wordScan ? (=="true") >-> (\x -> Bool True) ! wordScan ? (=="false") >-> (\x -> Bool False)
    

stmtParse = (funCallParse  >>- (matchChar ';') >-> (\x -> FunCall_ x)) ! curlyParse ! ifElseParse ! returnParse ! assignParse ! whileParse

curlyParse = matchChar '{' >>| iter stmtParse >>- matchChar '}' >-> (\x -> Seq x)

ifElseParse :: Parser Stmt
ifElseParse = ((wordScan ? (=="if")) >>| (matchChar '(') >>| expParse  >>- (matchChar ')') # stmtParse >-> (\(e,s) -> (If e s))) /?\ \(If e s) -> (wordScan ? (=="else")) >>| stmtParse >-> (\c -> (IfElse e s c))

assignParse :: Parser Stmt
assignParse = identScan >>- (matchChar '=') # expParse >>- parseEnd >-> (\(x,y) -> Assign x y)

whileParse = (wordScan ? (=="while")) >>| (matchChar '(') >>| expParse  >>- (matchChar ')') # stmtParse >-> (\(x,y) -> While x y)

returnParse =  (wordScan ? (=="return")) >>| expParse >>- parseEnd >-> (\x -> Return x)

-- not parsing custom type id yet, not sure why we should
typeParse = wordScan ? (=="int") >-> (\_ -> Int_) ! wordScan ? (=="int") >-> (\_ -> Bool_) ! parseTuple ! parseList
    where
    parseTuple = matchChar '(' >>| typeParse >>- matchChar ',' # typeParse >>- matchChar ')' >-> (\(x,y) -> Tuple_ x y)
    parseList = matchChar '[' >>| typeParse >>- matchChar ']' >-> (\x -> List_ x)

parseEnd = matchChar ';'
