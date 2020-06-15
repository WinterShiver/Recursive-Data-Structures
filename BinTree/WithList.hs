module BinTree.WithList where
-- module BinTree.WithList (isJust, isNothing, bt2list, list2bt) where

import BinTree
import BinTree.Complete
import BinTree.Traversal

-- Basics

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- Part 1. From BinTree to List of Maybe things that
{-
Sample Input: 
    1
   / \
  2   10 
 /     \
4       15

Sample Output: [Just 1,Just 2,Just 10,Just 4,Nothing,Nothing,Just 15]
-}

-- Step 1. from BinTree to perfect BinTree

-- bt2pbtMaybe :: BinTree a -> Pbt (Maybe a)
-- from a normal BinTree of a, to a perfect BinTree of Maybe a
-- the depth remains the same, but the tree is filled to be perfect
-- empty places are filled by `Nothing`
-- value of nodes are replaced by `Just k`
{-
Sample Input: 
    1
   / \
  2   10 
 /     \
4       15

Sample Output:
         Just 1
        /      \
  Just 2        Just 10 
 /      \       /      \
Just 4 Nothing Nothing Just 15
-}

bt2pbtMaybe :: BinTree a -> Pbt (Maybe a)
bt2pbtMaybe Empty = Empty
bt2pbtMaybe tree = fill (tree, depth tree) where
    fill (_, 0) = Empty
    fill (Empty, n) = Node (fill (Empty, n-1)) Nothing (fill (Empty, n-1))
    fill (Node l k r, n) = Node (fill (l, n-1)) (Just k) (fill (r, n-1))

-- Step 2. from perfect BinTree to List: 
--   2.1 tree -> level order traversal -> list: levelOrderTraversal
--   2.2 remove redundant Nothings from tail: removeTailNothings
-- Note that 2.1 make sense for all cbts (and also pbts, because
-- perfect is stronger than complete). Therefore levelOrderTraversal
-- in 2.1 is wrapped into cbt2list.
-- 2.1 and 2.2 are integrated in pbtMaybe2list.

cbt2list :: Cbt a -> [a]
cbt2list = levelOrderTraversal

pbtMaybe2list :: Pbt (Maybe a) -> [Maybe a]
pbtMaybe2list = removeTailNothings . cbt2list where
    removeTailNothings = reverse . removeHeadNothings . reverse
    removeHeadNothings = dropWhile isNothing

-- The above 2 steps are intergrated into bt2list

bt2list :: BinTree a -> [Maybe a]
bt2list = pbtMaybe2list . bt2pbtMaybe


-- Part 2. from list of `Maybe a` to Bintree of `a`. 
-- Assert the list is capable to compose a tree.

-- Step 1. Construct a complete bintree of `Maybe a` from the list
-- This process is done by list2cbt, which sequentially insert the list
-- elements into an empty tree, and in the process of inserting, 
-- the tree is always a complete tree.
-- Note that this process makes sense for all cbts. Therefore we provide
-- constructCbtFromList as the implementation, and spicified the process
-- by list2cbtMaybe to be utilized in this problem.

-- insertCbt: insert an elem into a BinTree of `Mayb`
insertCbt :: a -> Cbt a -> Cbt a
insertCbt m Empty = leaf m 
insertCbt m (Node l k r) 
    | empty l || not (perfect l) = Node (insertCbt m l) k r
    | empty r || not (perfect r) = Node l k (insertCbt m r)
    | depth l /= depth r = Node l k (insertCbt m r)
    | otherwise = Node (insertCbt m l) k r

-- constructCbtFromList: construct a cbt from the list
constructCbtFromList :: [a] -> Cbt a
constructCbtFromList = foldl (flip insertCbt) Empty

-- constructCbtFromList successfully gives the result of cbt 
-- > goodShow $ constructCbtFromList [Just 1,Just 2,Just 10,Just 4,Nothing,Nothing,Just 15]
-- "(Node (Node (Leaf Just 4) Just 2 (Leaf Nothing)) Just 1 (Node (Leaf Nothing) Just 10 (Leaf Just 15)))"
-- > goodShow $ constructCbtFromList [Just 1,Just 2,Just 10,Just 6,Nothing,Nothing,Just 15,Nothing,Just 4]
-- "(Node (Node (Node (Leaf Nothing) Just 6 (Leaf Just 4)) Just 2 (Leaf Nothing)) Just 1 (Node (Leaf Nothing) Just 10 (Leaf Just 15)))"

list2cbtMaybe :: [Maybe a] -> Cbt (Maybe a)
list2cbtMaybe = constructCbtFromList

-- Step 2. Translate the cbt of `Maybe a` to an ordinary tree
-- This process is done by replacing all the `Nothing`s in the cbt with
-- `Empty`, and replacing all the `Just k`s with `k`.

cbtMaybe2bt :: Cbt (Maybe a) -> BinTree a
cbtMaybe2bt Empty = Empty
cbtMaybe2bt (Node l k r) = case k of
    Nothing -> Empty
    Just k' -> Node (cbtMaybe2bt l) k' (cbtMaybe2bt r)

-- These two steps are intergrated as follows

list2bt :: [Maybe a] -> BinTree a
list2bt = cbtMaybe2bt . list2cbtMaybe

-- > goodShow $ list2bt [Just 1,Just 2,Just 10,Just 6,Nothing,Nothing,Just 15,Nothing,Just 4]
-- "(Node (Node (Node Empty 6 (Leaf 4)) 2 Empty) 1 (Node Empty 10 (Leaf 15)))"
-- > goodShow $ list2bt [Just 1,Just 2,Just 10,Just 6,Nothing,Nothing,Just 15]
-- "(Node (Node (Leaf 6) 2 Empty) 1 (Node Empty 10 (Leaf 15)))"


