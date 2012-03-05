module Scanner where
-- ?: Scan with p and check the parsed value with b, p ? b
-- !: Scan with a, if fails, parse with b, a ! b
-- #: Scan with a, parse leftovers with b, a # b
-- >->: Convert parsed type a to b
-- >>>: Extract a scanners result
-- >>|: Sequence operator that discards the first result
-- >>-: Discards second result

import Char
import Combinators
import AST

data Token = Int__ Int | Id__ String | String_ String
type Scanner a = CoreScanner a String

instance Show Token where
   show (Int__ x) = "<" ++ show x ++ ">"
   show (Id__ x) = "<" ++ x ++ ">"
   show (String_ x) = x
   
twoChar = token $ next # next >-> cat2

-- converts a string to int. Haskell doensn't have this function, strangely
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

-- TODO: add support for minus sign, or is this already taken care of by tokList?
numberScan :: Scanner Token
numberScan = (token (iter digitScan) ? (\x -> x /= [])) >-> (\x -> Int__ (toNum x))

alphaScan = next ? isAlpha
digitScan = next ? isDigit
spaceScan = matchCharList "\t\r\n "

-- note: no comments at the end of the file
commentScan x = iter ((twoChar ? (=="//")) # iter (next ? (/='\n'))) >>| x
alphaNumUnderScoreScan :: Scanner Char
alphaNumUnderScoreScan = next ? (\x -> isAlphaNum x || x == '_')
matchChar c = next ? (==c)
matchCharList cs = next ? (flip elem cs)

cat1 :: ([a], [a]) -> [a]
cat1 (hd, tl) = hd++tl

cat2 :: (a, a) -> [a]
cat2 (hd, snd) = [hd , snd]

-- note: keywords are also parsed as ids!
identScan :: Scanner Token
identScan = (token(alphaScan # iter alphaNumUnderScoreScan)) >-> (\x->Id__ (cat x))

-- discards the white spaces before and after the parsed result
token :: Scanner a -> Scanner a
token x = iter spaceScan >>| x >>- iter spaceScan
  
tokList = ["+", "-", "*", "/", "%", "==", "<", "<", ">", "<=", ">=", "!=", "&&", "||", ":", "!", "=", "(", ")", ";", "}", "{", ",", "[", "]"]

tokScan = (token ((twoChar ? inList) ! ((next >-> (\x -> [x])) ? inList))) >-> (\x->String_ x)
  where
  inList x = elem x tokList
  
lineScan :: Scanner [Token]
lineScan = iter (commentScan (tokScan ! identScan ! numberScan))
