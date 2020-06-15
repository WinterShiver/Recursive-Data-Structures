module BinTree.WithListCheck where

import Test.QuickCheck

import BinTree
import BinTree.Complete
import BinTree.Traversal
import BinTree.WithList

prop_complete_levelOrderTraversal :: BinTree Integer -> Bool
prop_complete_levelOrderTraversal tree = 
    complete tree || bt2list tree /= map Just (levelOrderTraversal tree)

prop_complete :: BinTree Integer -> Bool
prop_complete tree = 
    complete tree == isJust (sequenceA $ bt2list tree) 

prop_perfect :: BinTree Integer -> Bool
prop_perfect tree = 
    perfect tree == (complete tree && 
        2 ^ depth tree - 1 == length (bt2list tree)
    )

prop_complete_insert :: [Integer] -> Bool
prop_complete_insert xs = complete (constructCbtFromList xs)

prop_WithList :: BinTree Integer -> Bool
prop_WithList tree = (tree == (list2bt . bt2list) tree) 
    && (lst == (bt2list . list2bt) lst) where 
        lst = bt2list tree

-- check :: IO ()
-- check = quickCheck prop_WithList