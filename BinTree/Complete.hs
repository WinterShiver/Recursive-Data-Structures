-- this module is about the containing status of binary trees.

module BinTree.Complete where

import BinTree 

-- Containing status

-- empty binary tree: tree without any node in it. (defined in Tree.BinTree)
-- full binary tree: each node has 0 or 2 children, rather than 1 child
-- complete binary tree: each level, except the bottom level, is filled;
--    and the bottom level is filled from left to right.
-- perfect binary tree: level=n and numNode=2^n-1

full :: BinTree a -> Bool
full Empty = True
full (Node l k r)
    | empty l && empty r = True
    | not (empty l) && not (empty r) = full l && full r
    | otherwise = False

complete :: BinTree a -> Bool
complete Empty = True
complete (Node l k r) 
    | depth l == depth r = perfect l && complete r
    | depth l == succ (depth r) = complete l && perfect r
    | otherwise = False

perfect :: BinTree a -> Bool
perfect Empty = True
perfect (Node l k r) = depth l == depth r && perfect l && perfect r

type Fbt = BinTree -- bintree + full
type Cbt = BinTree -- bintree + complete
type Pbt = Cbt -- bintree + prefect
-- Note that perfect < complete, and perfect < full.

-- toFbt :: BinTree a -> Fbt a
-- toFbt tree = tree