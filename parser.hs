module Parser where

import Scanner
import Grammar
import Char

type Parser a = String -> Maybe (a, String)

-- converts a string to int. Haskell doensn't have this function, strangely
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

numberScan :: Scanner Int
numberScan = (iter digitScan) >-> toNum

--var :: Parser Exp
--var = token wordScan # token opScan # token numberScan >-> buildVarDecl

mulOp = token (matchChar '*') >-> (\_ -> (*))
	! token (matchChar '/') >-> (\_ -> (/))

addOp = token (matchChar '+') >-> (\_ -> (+))
	! token (matchChar '-')  >-> (\_ -> (-))

op = mulOp ! addOp	

--exp = numberScan op numberScan 
	
buildOp e (op, e') = op e e'