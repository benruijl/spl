import Char

-- parser converts a string to a tuple of what's parsed and the remaining string
type Scanner a = String -> Maybe (a, String)

-- Get character
char :: Scanner Char
char (c:cs) = Just(c,cs)
char [] = Nothing

-- Returns (a,cs)
tuple :: a -> Scanner a
tuple a cs = Just(a,cs)

-- Parse and check result
parseCheck :: (Scanner a) -> (a -> Bool) -> Scanner a
parseCheck p b cs = 
  case p cs of
     Nothing -> Nothing -- parsing failed
     Just (c, cs) -> if b c then Just(c,cs) else Nothing

-- A or B: Tries parsing with parser A. If it succeeds, it returns. Else parser B is tried. 
orParse :: Scanner a -> Scanner a -> Scanner a
orParse a b cs = 
  case a cs of
    Nothing -> b cs
    acs -> acs

charParse :: Scanner Char
charParse [] = Nothing
charParse (c:cs) = Just(c,cs)

alphaParse = parseCheck charParse isAlpha
digitParse = parseCheck charParse isDigit 

checkChar :: Char -> Scanner Char
checkChar c = parseCheck charParse (==c)

-- feed the result from parser A to parser B
chainParse :: Scanner a -> Scanner b -> Scanner (a,b)
chainParse a b cs =
  case a cs of
    Nothing -> Nothing
    Just(c, cs') -> 
      case b cs' of
        Nothing -> Nothing
        Just(q,cs'') -> Just((c,q),cs'')

-- converts a parsed expression to another type
convert :: Scanner a -> (a -> b) -> Scanner b
convert p k cs = 
  case p cs of
    Nothing -> Nothing
    Just(a,cs') -> Just(k a, cs')

cat :: (a, [a]) -> [a]
cat (hd, tl) = hd:tl

-- iterate parsing until an error is met
iter :: Scanner a -> Scanner [a]
iter p = orParse ( convert (chainParse p (iter p)) cat ) (tuple [])

-- converts a string to int. Haskell doensn't have this function, strangely
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

numberParse :: Scanner Int
numberParse = convert (iter digitParse) toNum

wordParse :: Scanner String
wordParse = iter alphaParse  

-- parses an identity
identParse :: Scanner String
identParse (c:cs) = 
  case isAlpha c of
        False -> Nothing
        True -> charScan [c] cs 
  where
    charScan x [] = Just(x, "")
    charScan x (c:cs) 
      | isSpace c = Just (x, (c:cs))
      | otherwise = charScan (x ++ [c]) cs

