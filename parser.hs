module Parser where

import TreeLib
import GrammarLib

t1 = Node 10 Tip Tip
t2 = Node 17 (Node 12 (Node 5 Tip (leaf 8)) (leaf 15))
              (Node 115
                    (Node 32 (leaf 30) (Node 46 Tip (leaf 57)))
                    (leaf 163))
main = do pict t2

-- probably need for overloaded conversion function 'parse' that pattern matches on arguments like toTuple (x,y) = Tuple x y but may not be so compositional;
-- in the end should declare Prog =... and main = do Prog;
-- for flexibility grammar should be placed in a separate file but not a .txt file in order to be properly checked by haskell for type consistency;