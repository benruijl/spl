module Scanner where
-- ?: Scan with p and check the parsed value with b, p ? b
-- !: Scan with a, if fails, parse with b, a ! b
-- #: Scan with a, parse leftovers with b, a # b
-- >->: Convert parsed type a to b
-- >>>: Extract a scanners result
-- >>|: Sequence operator that discards the first result
-- >>-: Discards second result

import Char

-- scanner converts a string to a tuple of what's parsed and the remaining string
type Scanner a = String -> Maybe (a, String)

-- Get character
char :: Scanner Char
char (c:cs) = Just(c,cs)
char [] = Nothing

twoChar =  token (char # char)        -- ONE TOKEN

-- Returns (a,cs)
tuple :: a -> Scanner a
tuple a cs = Just(a,cs)

-- converts a string to int. Haskell doensn't have this function, strangely
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

numberScan :: Scanner Int             -- ONE TOKEN
numberScan = token (iter digitScan) >-> toNum

-- Scan and check result
infixl 7 ?
(?) :: (Scanner a) -> (a -> Bool) -> Scanner a
(?) p b cs = 
  case p cs of
     Nothing -> Nothing -- parsing failed
     Just (c, cs) -> if b c then Just(c,cs) else Nothing

-- A or B: Tries parsing with scanner A. If it succeeds, it returns. Else scanner B is tried. 
infixl 3 !
(!) :: Scanner a -> Scanner a -> Scanner a
(!) a b cs = 
  case a cs of
    Nothing -> b cs
    acs -> acs

alphaScan = char ? isAlpha
digitScan = char ? isDigit
spaceScan = char ? isSpace ! (matchChar '\t') ! (matchChar '\r') ! (matchChar '\n')
alphaNumUnderScoreScan :: Scanner Char
alphaNumUnderScoreScan = token(char ? (\x -> isAlphaNum x  || x == '_'))   -- ONE TOKEN
matchChar c = char ? (==c)  -- ONE TOKEN

-- feed the result from scanner A to scanner B (chainScan)
infixl 6 #
(#) :: Scanner a -> Scanner b -> Scanner (a,b)
(#) a b cs =
  case a cs of
    Nothing -> Nothing
    Just(c, cs') -> 
      case b cs' of
        Nothing -> Nothing
        Just(q,cs'') -> Just((c,q),cs'')

infixr 6 /\
(/\) :: Scanner a -> (a -> Scanner b) -> Scanner b
(/\) p q cs = case p cs of
	Nothing -> Nothing
	Just (c,cs') -> q c cs'

infix 6 /?\
(/?\) :: Scanner a -> (a -> Scanner a) -> Scanner a
(/?\) op1 op2 = op1 /\ (\l -> (op2 l) ! (tuple l))

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

wordScan :: Scanner String             -- ONE TOKEN
wordScan = token(iter alphaScan)  

-- Extract a scanners result
infix 4 >>>
(>>>) :: Scanner a -> (a -> Scanner b) -> Scanner b
(m >>> k) cs = case m cs of
	Nothing -> Nothing
	Just (a, cs') -> k a cs'

-- Sequence operator that discards the first result 
infixl 6 >>| 
(>>|) :: Scanner a -> Scanner b -> Scanner b
(m >>| n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> n cs'

infixl 6 >>-  -- Discards second result
(>>-) :: Scanner a -> Scanner b -> Scanner a
(m >>- n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(a, cs2)

-- discards the white spaces before and after the parsed result
token :: Scanner a -> Scanner a
token x = iter spaceScan >>| x >>- (iter spaceScan)
  
tokList = ["+", "-", "*", "/", "%", "==", "<", "<", ">", "<=", ">=", "!=", "&&", "||", ":", "!", "=", "(", ")", ";", "}", "{"]

tokScan = token ((twoChar >-> cat2) ? inList) ! ((char >-> (\x -> [x])) ? inList) -- ONE TOKEN
  where
  inList x = elem x tokList
