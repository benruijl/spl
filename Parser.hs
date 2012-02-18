module Parser where

import Scanner
import AST
import Char

type Parser a = Scanner a

-- converts a string to int. Haskell doensn't have this function, strangely
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

numberScan :: Scanner Int
numberScan = (iter digitScan) >-> toNum

boolScan :: Scanner Bool
trueScan = wordScan ? (=="True") >-> (\_-> True)
falseScan = wordScan ?(=="False") >-> (\_-> False)
boolScan = trueScan ! falseScan 
--var :: Parser Exp
--var = token wordScan # token opScan # token numberScan >-> buildVarDecl

opAdd = token (matchChar '+') >-> (\_ -> Add)
opSub = token (matchChar '-') >-> (\_ -> Sub)
opMult = token (matchChar '*') >-> (\_ -> Mult)
opDiv  = token (matchChar '/') >-> (\_ -> Div)
opMod = token (matchChar '%') >-> (\_ -> Mod)
opEquals = token (matchChar '=') # token (matchChar '=') >-> (\_ -> Equals)
opLess = token (matchChar '<') >-> (\_ -> Less)
opMore = token (matchChar '>') >-> (\_ -> More)
opLessEq = token (matchChar '<') # token (matchChar '=') >-> (\_ -> LessEq)
opMoreEq = token (matchChar '>') # token (matchChar '=') >-> (\_ -> MoreEq)
opNotEq = token (matchChar '!') # token (matchChar '=') >-> (\_ -> NotEq)
opAnd = token (matchChar '&') # token (matchChar '&') >-> (\_ -> And)
opOr = token (matchChar '|') # token (matchChar '|') >-> (\_ -> Or)
opAppCons = token (matchChar ':') >-> (\_ -> AppCons)
opNegate = token (matchChar '!') >-> (\_ -> Negate)
opUnitaryMinus = token (matchChar '-') >-> (\_ -> UnitaryMinus)

-- TODO: put in good category
op2 = opEquals ! opLessEq ! opMoreEq ! opNotEq ! opAdd ! opSub ! opMod ! opLess ! opMore ! opAnd ! opOr ! opAppCons
term = opMult ! opDiv

op1 = opNegate ! opUnitaryMinus 

identScan = (alphaScan >-> (\x -> [x])) # (iter alphaNumUnderScoreScan) >-> cat1

intScan = ((((matchChar '-') >-> (\x->[x])) # (iter digitScan)) >-> cat1 ! (iter digitScan) ? (/="")) >-> (\x -> Int (toNum x))

fArgsParse :: Parser FArgs
fArgsParse = ((token typeParse # identScan) >-> (\x -> [x])) /?\ (\x -> (matchChar ',') >>| fArgsParse >-> (\y -> x ++ y))

-- accepts empty functions. add check here or check later?
funDeclParse :: Parser FunDecl
funDeclParse = (token retTypeParse # identScan >>- (matchChar '(') # (fArgsParse ! tuple []) >>- (matchChar ')') >>- (matchChar '{') # (iter varDeclParse) # (iter stmtParse) >>- (matchChar '}')) >-> (\((((t,i),a),v),s) -> FD t i a v s)

funCallParse :: Parser FunCall
funCallParse = identScan >>- (matchChar '(') # (actArgsParse ! tuple []) >>- (matchChar ')')

actArgsParse :: Parser ActArgs
actArgsParse = (expParse >-> (\x -> [x])) /?\ (\x -> (matchChar ',') >>| actArgsParse >-> (\y -> x ++ y))

retTypeParse :: Parser RetType
retTypeParse = typeParse >-> (\x -> Type x) ! wordScan ? (=="void") >-> (\x -> Void)

-- note: each variable has to be initialised!
varDeclParse :: Parser VarDecl
varDeclParse = (token typeParse # identScan >>- (matchChar '=') # expParse >>- (matchChar ';')) >-> (\((x, y),z) -> VD x y z)

expParse = (fcParse ! tParse ! boolParse ! tupleParse ! parParse ! emptyListParse) /?\ op2Parse
   where
    tParse = termParse >-> (\x -> Term_ x)
    fcParse = funCallParse >-> (\x -> FunCall x)
    boolParse = wordScan ? (=="true") >-> (\x -> Bool True) ! wordScan ? (=="false") >-> (\x -> Bool False)
    op2Parse :: Exp -> Parser Exp
    op2Parse x = (op2 # termParse >-> (\(o,y) -> ExpOp_ o x y))  /?\ op2Parse
    emptyListParse :: Parser Exp
    emptyListParse = matchChar '[' >>- matchChar ']' >-> (\_ -> EmptyList)
    tupleParse :: Parser Exp
    tupleParse = matchChar '(' >>| expParse >>- matchChar ',' # expParse >>- matchChar ')' >-> (\(x,y) -> Tuple x y)
    parParse :: Parser Exp
    parParse = matchChar '(' >>| expParse >>- matchChar ')'
    
termParse :: Parser Term
termParse = (factorParse >-> (\x -> Factor x)) /?\ term2Parse
   where
   term2Parse :: Term -> Parser Term
   term2Parse x = (term # factorParse >-> (\(o,y) -> TermOp o x y))  /?\ term2Parse
   factorParse :: Parser Factor
   factorParse = (identScan >-> (\x -> Id x)) ! intScan
    

stmtParse = (funCallParse  >>- (matchChar ';') >-> (\x -> FunCall_ x)) ! curlyParse ! ifElseParse ! ifParse ! returnParse ! assignParse ! whileParse

curlyParse = matchChar '{' >>| iter stmtParse >>- matchChar '}' >-> (\x -> List x)

ifParse :: Parser Stmt
ifParse = (wordScan ? (=="if")) >>| (matchChar '(') >>| expParse  >>- (matchChar ')') # stmtParse >-> (\(x,y) -> If x y)

-- todo: merge with ifParse to prevent reparsing
ifElseParse :: Parser Stmt
ifElseParse = ifParse >>- (wordScan ? (=="else")) # stmtParse >-> (\(If x y,z) -> IfElse x y z)


assignParse :: Parser Stmt
assignParse = identScan >>- (matchChar '=') # expParse >>- parseEnd >-> (\(x,y) -> Assign x y)

whileParse = (wordScan ? (=="while")) >>| (matchChar '(') >>| expParse  >>- (matchChar ')') # stmtParse >-> (\(x,y) -> While x y)

returnParse = token (wordScan ? (=="return")) >>| expParse >>- parseEnd >-> (\x -> Return x)

-- not parsing custom type id yet, not sure why we should
typeParse = read "int" >-> (\_ -> Int_) ! read "bool" >-> (\_ -> Bool_) ! parseTuple ! parseList
    where
    parseTuple = matchChar '(' >>| typeParse >>- matchChar ',' # typeParse >>- matchChar ')' >-> (\(x,y) -> Tuple_ x y)
    parseList = matchChar '[' >>| typeParse >>- matchChar ']' >-> (\x -> List_ x)
    read x =  token $ wordScan ? (==x)

parseEnd = matchChar ';'
