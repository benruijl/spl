module TreeLib where

data Tree a = Tip | Node a (Tree a) (Tree a) deriving (Show,Eq)
leaf x = Node x Tip Tip

size Tip = 0
size (Node _ tl tr) = 1 + size tl + size tr

-- Finds height of tree
height Tip = 0
height (Node _ xl xr) = 1 + max (height xl) (height xr)

-- Finds farthest left item.  In a BST this is the smallest value
farLft (Node x Tip _) = x
farLft (Node x xl _) = farLft xl

-- Finds farthest right item.  In a BST this is the largest value
farRt (Node x _ Tip) = x
farRt (Node x _ xr) = farRt xr

-- Mirrors a tree, so every right and left node switch
mirror Tip = Tip
mirror (Node x xl xr) = Node x (mirror xr) (mirror xl)

-- Adds up all items in a numerical Binary Tree
-- h is a function which combines two items - default would be (+)
sumt Tip h = 0
sumt (Node x xl xr) h = h (h x (sumt xl h)) (sumt xr h)

-- The third parameter is a comparator function, if the contents of the tree are not numbers or strings, you can pass a different function to this third parameter to properly define how to compare, and therefore sort, the tree.
insert a Tip _ = leaf a
insert a (Node x xl xr) f | f a x     = Node x (insert a xl f) xr
                          | otherwise = Node x xl (insert a xr f)

treeToList Tip = []
treeToList (Node x xl xr) = x : treeToList xl ++ treeToList xr

-- Finds smalles item in a tree (not necessarily ordered) using comparator f
-- This function does not deal with a tree which is just a tip

mint (Node x Tip Tip) f = x
mint (Node x xl Tip) f  | f x xlSm  = x
                        | otherwise = xlSm
                        where xlSm = mint xl f
mint (Node x Tip xr) f  | f x xrSm  = x
                        | otherwise = xrSm
                        where xrSm = mint xr f
mint (Node x xl xr) f   | f x xlSm && f x xrSm = x
                        | f xlSm xrSm          = xlSm
                        | otherwise            = xrSm
                        where xlSm = mint xl f
                              xrSm = mint xr f
							  
-- Finds largest item in a tree (not necessarily ordered) using comparator f
-- flips the parameters of f in order to find the largest using a smaller-than comparator
-- This function does not deal with a tree which is just a tip

maxt (Node x Tip Tip) f = x
maxt (Node x xl Tip) f  | (flip f) x xlSm = x
                        | otherwise       = xlSm
                        where xlSm = maxt xl f
maxt (Node x Tip xr) f  | (flip f) x xrSm = x
                        | otherwise       = xrSm
                        where xrSm = maxt xr f
maxt (Node x xl xr) f   | (flip f) x xlSm && (flip f) x xrSm = x
                        | (flip f) xlSm xrSm                 = xlSm
                        | otherwise                          = xrSm
                        where xlSm = maxt xl f
                              xrSm = maxt xr f

-- Visually outputs a tree (sideways) by indenting each next node and additional tab in, outputs the right side, then the node, then the left side
pict t = putStr (pic "" t)
         where pic ind Tip = ind ++ "."
               pic ind (Node x tl tr) = pic ('\t':ind) tr ++ "\n" ++
                                        ind ++ show x     ++ "\n" ++
                                        pic ('\t':ind) tl ++ "\n"
										
-- Converts a list to a tree, using either comparator function f, or the natural (<=) function
listToTree ls f = foldl (\tre x -> insert x tre f) Tip ls

-- Tree Sort - takes a list, and sorts it by converting to a tree and then back to a list
tsort ls f = treeToListOrd (listToTree ls f)	  
							  
				
-- Converts a tree into a sorted list.  This list will not convert back into the same tree this time !
treeToListOrd Tip = []
treeToListOrd (Node x xl xr) = treeToListOrd xl ++ x : treeToListOrd xr