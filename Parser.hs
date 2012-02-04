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
