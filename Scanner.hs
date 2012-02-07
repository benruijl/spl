-- ?: Scan with p and check the parsed value with b, p ? b
-- !: Scan with a, if fails, parse with b, a ! b
-- #: Scan with a, parse leftovers with b, a # b
-- >->: Convert parsed type a to b

import Char

-- parser converts a string to a tuple of what's parsed and the remaining string
type Scanner a = String -> Maybe (a, String)

-- Get character
char :: Scanner Char
char (c:cs) = Just(c,cs)
char [] = Nothing

twoChar = char # char

-- Returns (a,cs)
tuple :: a -> Scanner a
tuple a cs = Just(a,cs)

-- Scan and check result
infixl 7 ?
(?) :: (Scanner a) -> (a -> Bool) -> Scanner a
(?) p b cs = 
  case p cs of
     Nothing -> Nothing -- parsing failed
     Just (c, cs) -> if b c then Just(c,cs) else Nothing

-- A or B: Tries parsing with parser A. If it succeeds, it returns. Else parser B is tried. 
infixl 3 !
(!) :: Scanner a -> Scanner a -> Scanner a
(!) a b cs = 
  case a cs of
    Nothing -> b cs
    acs -> acs

alphaScan = char ? isAlpha
digitScan = char ? isDigit
alphaNumUnderScoreScan :: Scanner Char
alphaNumUnderScoreScan = char ? (\x -> isAlphaNum x  || x == '_')
matchChar c = char ? (==c)

-- feed the result from parser A to parser B (chainScan)
infixl 6 #
(#) :: Scanner a -> Scanner b -> Scanner (a,b)
(#) a b cs =
  case a cs of
    Nothing -> Nothing
    Just(c, cs') -> 
      case b cs' of
        Nothing -> Nothing
        Just(q,cs'') -> Just((c,q),cs'')

-- converts a parsed expression to another type
infixl 5 >->
(>->) :: Scanner a -> (a -> b) -> Scanner b
(>->) p k cs = 
  case p cs of
    Nothing -> Nothing
    Just(a,cs') -> Just(k a, cs')

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

-- converts a string to int. Haskell doensn't have this function, strangely
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

numberScan :: Scanner Int
numberScan = (iter digitScan) >-> toNum

wordScan :: Scanner String
wordScan = iter alphaScan  


identScan = (alphaScan >-> (\x -> [x])) # (iter alphaNumUnderScoreScan) >-> cat1

opList = ["+", "-", "*", "/", "%", "==", "<", "<", ">", "<=", ">=", "!=", "&&", "||", ":", "!", "="]

opScan = ((twoChar >-> cat2) ? inList) ! ((char >-> (\x -> [x])) ? inList) 
  where
  inList x = elem x opList

-- note: intScan returns an empty string on total failure instead of nothing
intScan = (((matchChar '-') >-> (\x->[x])) # (iter digitScan)) >-> cat1 ! (iter digitScan)

-- line scan, space not supported. 
-- lineScan "a<=528;" gets parsed correctly
lineScan = iter (identScan ! opScan ! (intScan ? (/="")))

