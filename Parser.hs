import Char
-- for testing, this is a standalone module that tries to parse variable assigments.
type Id = String

data Type = Num Integer | Bool Bool | Tuple Type Type | List Type deriving Show
data Decl = Type Id

-- parser converts a string to a tuple of what's parsed and the remaining string
type Parser a = String -> Maybe (a, String)

-- Get character
char :: Parser Char
char (c:cs) = Just(c,cs)
char [] = Nothing

-- Returns (a,cs)
tuple :: a -> Parser a
tuple a cs = Just(a,cs)

-- Parse and check result
parseCheck :: (Parser a) -> (a -> Bool) -> Parser a
parseCheck p b cs = 
  case p cs of
     Nothing -> Nothing -- parsing failed
     Just (c, cs) -> if b c then Just(c,cs) else Nothing

-- A or B: Tries parsing with parser A. If it succeeds, it returns. Else parser B is tried. 
orParse :: Parser a -> Parser a -> Parser a
orParse a b cs = 
  case a cs of
    Nothing -> b cs
    acs -> acs

charParse :: Parser Char
charParse [] = Nothing
charParse (c:cs) = Just(c,cs)

alphaParse = parseCheck charParse isAlpha
digitParse = parseCheck charParse isDigit 

checkChar :: Char -> Parser Char
checkChar c = parseCheck charParse (==c)

-- feed the result from parser A to parser B
chainParse :: Parser a -> Parser b -> Parser (a,b)
chainParse a b cs =
  case a cs of
    Nothing -> Nothing
    Just(c, cs') -> 
      case b cs' of
        Nothing -> Nothing
        Just(q,cs'') -> Just((c,q),cs'')

-- converts a parsed expression to another type
convert :: Parser a -> (a -> b) -> Parser b
convert p k cs = 
  case p cs of
    Nothing -> Nothing
    Just(a,cs') -> Just(k a, cs')

cat :: (a, [a]) -> [a]
cat (hd, tl) = hd:tl

-- iterate parsing until an error is met
iter :: Parser a -> Parser [a]
iter p = orParse ( convert (chainParse p (iter p)) cat ) (tuple [])

-- converts a string to int. Haskell doensn't have this function, strangely
toNum :: [Char] -> Int
toNum = foldl (\x y -> 10 * x + (digitToInt y)) 0

numberParse :: Parser Int
numberParse = convert (iter digitParse) toNum

wordParse :: Parser String
wordParse = iter alphaParse  
