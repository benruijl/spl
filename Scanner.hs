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

-- returns unfiltered character. could be a space
char (c:cs) = Just(c,cs)
char [] = Nothing

twoChar = token $ char # char       -- ONE TOKEN

-- converts a string to int. Haskell doensn't have this function, strangely
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

numberScan :: Scanner Int             -- ONE TOKEN
numberScan = token (iter digitScan) >-> toNum

alphaScan = char ? isAlpha
digitScan = char ? isDigit
spaceScan = matchCharList "\t\r\n "
alphaNumUnderScoreScan :: Scanner Char
alphaNumUnderScoreScan = char ? (\x -> isAlphaNum x  || x == '_')   -- ONE TOKEN
matchChar c = char ? (==c)  -- ONE TOKEN
matchCharList cs = char ? (flip elem cs)

cat :: (a, [a]) -> [a]
cat (hd, tl) = hd:tl

cat1 :: ([a], [a]) -> [a]
cat1 (hd, tl) = hd++tl

cat2 :: (a, a) -> [a]
cat2 (hd, snd) = [hd , snd]

-- iterate parsing until an error is met
-- warning: iter returns an empty list instead of Nothing
iter :: Scanner a -> Scanner [a]
iter p = (p # iter p) >-> cat ! tuple []

wordScan :: Scanner String             -- ONE TOKEN
wordScan = token(iter alphaScan)  

-- discards the white spaces before and after the parsed result
token :: Scanner a -> Scanner a
token x = iter spaceScan >>| x >>- iter spaceScan
  
tokList = ["+", "-", "*", "/", "%", "==", "<", "<", ">", "<=", ">=", "!=", "&&", "||", ":", "!", "=", "(", ")", ";", "}", "{"]

tokScan = token ((twoChar >-> cat2) ? inList) ! ((char >-> (\x -> [x])) ? inList) -- ONE TOKEN
  where
  inList x = elem x tokList
