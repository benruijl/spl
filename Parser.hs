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

op2 = opEquals ! opLessEq ! opMoreEq ! opNotEq ! opAdd ! opSub  ! opMult ! opDiv ! opMod ! opLess ! opMore ! opAnd ! opOr ! opAppCons
op1 = opNegate ! opUnitaryMinus 

identScan = (alphaScan >-> (\x -> [x])) # (iter alphaNumUnderScoreScan) >-> cat1

expParse = idParse
   where
    idParse :: Parser Exp
    idParse = (identScan >-> (\x -> Id x)) /?\ op2Parse
    op2Parse :: Exp -> Parser Exp
    op2Parse x = (op2 # expParse >-> (\(o,y) -> Op2_ o x y))  /?\ op2Parse
    

stmtParse = curlyParse ! ifElseParse ! ifParse ! returnParse ! assignParse ! whileParse

-- TODO: support for multiple statements
curlyParse = matchChar '{' >>| stmtParse >>- matchChar '}'

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
