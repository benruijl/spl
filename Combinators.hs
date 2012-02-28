module Combinators where
-- ?: Scan with p and check the parsed value with b, p ? b
-- !: Scan with a, if fails, parse with b, a ! b
-- #: Scan with a, parse leftovers with b, a # b
-- >->: Convert parsed type a to b
-- >>>: Extract a CoreScanners result
-- >>|: Sequence operator that discards the first result
-- >>-: Discards second result

-- CoreScanner converts a string to a tuple of what's parsed and the remaining string
type CoreScanner a b = b -> Maybe (a, b)

-- Returns (a,cs)
tuple :: a  -> CoreScanner a b
tuple a cs = Just(a,cs)

-- Scan and check result
infixl 7 ?
(?) :: (CoreScanner a b) -> (a -> Bool) -> CoreScanner a b
(?) p b cs =
  case p cs of
     Nothing -> Nothing -- parsing failed
     Just (c, cs) -> if b c then Just(c,cs) else Nothing

-- A or B: Tries parsing with CoreScanner A. If it succeeds, it returns. Else CoreScanner B is tried.
infixl 3 !
(!) :: CoreScanner a b -> CoreScanner a b -> CoreScanner a b
(!) a b cs =
  case a cs of
    Nothing -> b cs
    acs -> acs

-- feed the result from CoreScanner A to CoreScanner B (chainScan)

infixl 6 #
(#) :: CoreScanner a b -> CoreScanner c b -> CoreScanner (a, c) b 
(#) a b cs =
  case a cs of
    Nothing -> Nothing
    Just(c, cs') ->
      case b cs' of
        Nothing -> Nothing
        Just(q,cs'') -> Just((c,q),cs'')

infixr 6 /\
(/\) :: CoreScanner a b -> (a -> CoreScanner c b) -> CoreScanner c b
(/\) p q cs = case p cs of
	Nothing -> Nothing
	Just (c,cs') -> q c cs'

infix 6 /?\
(/?\) :: CoreScanner a b -> (a -> CoreScanner a b) -> CoreScanner a b
(/?\) op1 op2 = op1 /\ (\l -> (op2 l) ! (tuple l))

-- converts a parsed expression to another type
infixl 5 >->
(>->) :: CoreScanner a b -> (a -> c) -> CoreScanner c b
(>->) p k cs =
  case p cs of
    Nothing -> Nothing
    Just(a,cs') -> Just(k a, cs')

-- Extract a CoreScanners result
infix 4 >>>
(>>>) :: CoreScanner a b -> (a -> CoreScanner c b) -> CoreScanner c b
(m >>> k) cs = case m cs of
	Nothing -> Nothing
	Just (a, cs') -> k a cs'

-- Sequence operator that discards the first result
infixl 6 >>|
(>>|) :: CoreScanner a b -> CoreScanner c b -> CoreScanner c b
(m >>| n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> n cs'

infixl 6 >>- -- Discards second result
(>>-) :: CoreScanner a b -> CoreScanner c b -> CoreScanner a b
(m >>- n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(a, cs2)