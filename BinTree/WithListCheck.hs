module BinTree.WithListCheck where

import Test.QuickCheck

import BinTree
import BinTree.WithList

prop_list2bt :: BinTree Integer -> Bool
prop_list2bt tree = tree == (list2bt . bt2list) tree

check :: IO ()
check = quickCheck prop_list2bt