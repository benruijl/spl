module Combinators where

type M a b = b -> (a, b)

yield :: a -> b -> (a, b)
yield t e = (t, e)

infixl 5 >->
(>->) :: M a m -> (a -> b) -> M b m
(>->) p k e =
  case p e of
    (a,e') -> (k a, e')

-- mutate environment
infixr 4 >-->
(>-->) ::  (m -> m) -> M a m -> M a m
(>-->) k p e =
  case p e of
    (a,e') -> (a, k e')
    
infixl 6 #
(#) :: M a m -> M b m -> M (a,b) m
(#) a b e =
  case a e of
    (c, e') ->
      case b e' of
        (q, e'') -> ((c,q), e'')

infixl 6 !++!
(!++!) :: M a m -> (a -> M b m) -> M b m
(!++!) a b e =
  case a e of
    (c, e') -> b c e'

-- modify last processed part
infixl 6 !-+!
(!-+!) :: M (a,b) m -> (b -> M c m) -> M (a,c) m
(!-+!) a b e =
  case a e of
    ((c,d), e') -> (b d >-> \x -> (c, x)) e'

infixl 6 >>|
(>>|) :: M a m -> M b m -> M b m
(m >>| n) cs = case m cs of
    (a, cs') -> n cs'

infixl 6 >>- -- Discards second result
(>>-) :: M a m -> M b m -> M a m
(m >>- n) cs = case m cs of
    (a, cs') -> case n cs' of
        (b, cs2) -> (a, cs2)

iter :: (a -> M b m) -> [a] -> M [b] m
iter f [] = yield []
iter f (l:ls) = f l # iter f ls >-> (\(a,b) -> a:b)
