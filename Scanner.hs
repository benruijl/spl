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

data Token = Int__ Int | Id__ String | String_ String deriving Show
type Scanner a = CoreScanner a String

-- ?: Scan with p and check the parsed value with b, p ? b
-- !: Scan with a, if fails, parse with b, a ! b
-- #: Scan with a, parse leftovers with b, a # b
-- >->: Convert parsed type a to b
-- >>>: Extract a scanners result
-- >>|: Sequence operator that discards the first result
-- >>-: Discards second result

-- returns unfiltered character. could be a space

char (c:cs) = Just(c,cs)
char [] = Nothing

twoChar = token $ char # char >-> cat2

-- converts a string to int. Haskell doensn't have this function, strangely
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

numberScan :: Scanner Token -- ONE TOKEN the output is token so the first parameter
numberScan = token (iter digitScan) >-> (\x -> Int__ (toNum x))

alphaScan = char ? isAlpha
digitScan = char ? isDigit
spaceScan = matchCharList "\t\r\n "
alphaNumUnderScoreScan :: Scanner Char
alphaNumUnderScoreScan = char ? (\x -> isAlphaNum x || x == '_') -- ONE TOKEN
matchChar c = char ? (==c) -- ONE TOKEN
matchCharList cs = char ? (flip elem cs)

cat1 :: ([a], [a]) -> [a]
cat1 (hd, tl) = hd++tl

cat2 :: (a, a) -> [a]
cat2 (hd, snd) = [hd , snd]

identScan :: Scanner Token
identScan = token(alphaScan # iter alphaNumUnderScoreScan) >-> (\x->Id__ (cat x))


-- discards the white spaces before and after the parsed result
token :: Scanner a -> Scanner a
token x = iter spaceScan >>| x >>- iter spaceScan
  
tokList = ["+", "-", "*", "/", "%", "==", "<", "<", ">", "<=", ">=", "!=", "&&", "||", ":", "!", "=", "(", ")", ";", "}", "{"]

tokScan = (token ((twoChar ? inList) ! ((char >-> (\x -> [x])) ? inList))) >-> (\x->String_ x) -- ONE TOKEN
  where
  inList x = elem x tokList
  
lineScan :: Scanner [Token]
lineScan =  iter (identScan ! tokScan)-- ! numberScan)
