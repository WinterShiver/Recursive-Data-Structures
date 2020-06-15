module BinTree.Traversal where

import Data.Foldable -- toList

import BinTree

-- Traversals

preOrderTraversal :: BinTree a -> [a]
preOrderTraversal Empty = []
preOrderTraversal (Node l k r) = 
    k:preOrderTraversal l ++ preOrderTraversal r

inOrderTraversal :: BinTree a -> [a]
inOrderTraversal = toList

postOrderTraversal :: BinTree a -> [a]
postOrderTraversal Empty = []
postOrderTraversal (Node l k r) = 
    postOrderTraversal l ++ postOrderTraversal r ++ [k]

levelOrderTraversal :: BinTree a -> [a]
levelOrderTraversal tree = reverse $ step ([], [tree]) where 
    step (result, trees) = case trees of
        [] -> result
        Empty:ts -> step (result, ts)
        (Node l k r):ts -> step (k:result, ts++[l, r])

-- Test

testTraversal tree = map ($ tree) [
    preOrderTraversal, inOrderTraversal, postOrderTraversal
    , levelOrderTraversal
    ]