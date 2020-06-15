module BinTree.MulTree where

import BinTree hiding (depth)

-- MulTree: left son right brother

type MulTree = BinTree

-- Methods

depth :: MulTree a -> Int
depth Empty = 0
depth (Node l k r) = max (succ $ depth l) (depth r)