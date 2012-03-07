module Scanner where
-- ?: Scan with p and check the parsed value with b, p ? b
-- !: Scan with a, if fails, parse with b, a ! b
-- #: Scan with a, parse leftovers with b, a # b
-- >->: Convert parsed type a to b
-- >>>: Extract a scanners result
-- >>|: Sequence operator that discards the first result
-- >>-: Discards second result

import Data.Char
import Combinators
import AST

data Token = Int__ Int | Id__ String | String_ String
type Scanner a = CoreScanner a String

instance Show Token where
   show (Int__ x) = "<" ++ show x ++ ">"
   show (Id__ x) = "<" ++ x ++ ">"
   show (String_ x) = x
   
twoChar = token $ next # next >-> (\(a,b) -> [a, b])

-- converts a string to int
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

-- note: minus signs are parsed as unitary minus
numberScan :: Scanner Token
numberScan = (token (iter digitScan) ? (\x -> x /= [])) >-> (\x -> Int__ (toNum x))

alphaScan = next ? isAlpha
digitScan = next ? isDigit
spaceScan = matchCharList "\t\r\n "


-- note: no comments at the end of the file
commentScan x =  starScanner >>| slashScanner >>| x
		where
			starScanner = iter ((twoChar ? (=="/*")) # (iter ((next ? (/='*') >-> (\x -> [x])) ! (twoChar ? (/="*/"))) # twoChar))
			slashScanner = iter ((twoChar ? (=="//")) # (iter (next ? (/='\n'))))
			
alphaNumUnderScoreScan = next ? (\x -> isAlphaNum x || x == '_')
matchChar c = next ? (==c)
matchCharList cs = next ? (flip elem cs)

-- note: keywords are also parsed as ids!
identScan :: Scanner Token
identScan = (token(alphaScan # iter alphaNumUnderScoreScan)) >-> (\(x,s) -> Id__ (x : s))

-- discards the white spaces before and after the parsed result
token :: Scanner a -> Scanner a
token x = iter spaceScan >>| x >>- iter spaceScan

tokScan = (token ((twoChar ? inList) ! ((next >-> (\x -> [x])) ? inList))) >-> (\x->String_ x)
  where
  tokList = ["+", "-", "*", "/", "%", "==", "<", "<", ">", "<=", ">=", "!=", "&&", "||", ":", "!", "=", "(", ")", ";", "}", "{", ",", "[", "]"]
  inList x = elem x tokList
  
lineScan :: Scanner [Token]
lineScan = iter (commentScan (tokScan ! identScan ! numberScan))
