module Combinators where
-- ?: Scan with p and check the parsed value with b, p ? b
-- !: Scan with a, if fails, parse with b, a ! b
-- #: Scan with a, parse leftovers with b, a # b
-- >->: Convert parsed type a to b
-- >>>: Extract a scanners result
-- >>|: Sequence operator that discards the first result
-- >>-: Discards second result

-- scanner converts a string to a tuple of what's parsed and the remaining string
type Scanner a b = b -> Maybe (a, b)

-- Returns (a,cs)
tuple :: a  -> Scanner a b
tuple a cs = Just(a,cs)

-- Scan and check result
infixl 7 ?
(?) :: (Scanner a b) -> (a -> Bool) -> Scanner a b
(?) p b cs =
  case p cs of
     Nothing -> Nothing -- parsing failed
     Just (c, cs) -> if b c then Just(c,cs) else Nothing

-- A or B: Tries parsing with scanner A. If it succeeds, it returns. Else scanner B is tried.
infixl 3 !
(!) :: Scanner a b -> Scanner a b -> Scanner a b
(!) a b cs =
  case a cs of
    Nothing -> b cs
    acs -> acs

-- feed the result from scanner A to scanner B (chainScan)
{-
infixl 6 #
(#) :: Scanner a b -> Scanner c d -> Scanner ((a, c) (b, d))
(#) a b cs =
  case a cs of
    Nothing -> Nothing
    Just(c, cs') ->
      case b cs' of
        Nothing -> Nothing
        Just(q,cs'') -> Just((c,q),cs'')
-}

infixr 6 /\
(/\) :: Scanner a b -> (a -> Scanner c b) -> Scanner c b
(/\) p q cs = case p cs of
	Nothing -> Nothing
	Just (c,cs') -> q c cs'

infix 6 /?\
(/?\) :: Scanner a b -> (a -> Scanner a b) -> Scanner a b
(/?\) op1 op2 = op1 /\ (\l -> (op2 l) ! (tuple l))

-- converts a parsed expression to another type
infixl 5 >->
(>->) :: Scanner a b -> (a -> c) -> Scanner c b
(>->) p k cs =
  case p cs of
    Nothing -> Nothing
    Just(a,cs') -> Just(k a, cs')

-- Extract a scanners result
infix 4 >>>
(>>>) :: Scanner a b -> (a -> Scanner c b) -> Scanner c b
(m >>> k) cs = case m cs of
	Nothing -> Nothing
	Just (a, cs') -> k a cs'

-- Sequence operator that discards the first result
infixl 6 >>|
(>>|) :: Scanner a b -> Scanner c b -> Scanner c b
(m >>| n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> n cs'

infixl 6 >>- -- Discards second result
(>>-) :: Scanner a b -> Scanner c b -> Scanner a b
(m >>- n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(a, cs2)