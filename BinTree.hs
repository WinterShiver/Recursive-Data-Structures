module BinTree where

import Control.Applicative

import Test.QuickCheck

-- BinTree: binary branched tree

data BinTree a = Empty | Node (BinTree a) a (BinTree a) deriving (Show)

testBt1 = Node (Node (leaf 4) 2 Empty) 1 (Node Empty 10 (leaf 15))
{-
    1
   / \
  2   10 
 /     \
4       15
-}
testBt2 = Node (Node (Node Empty 6 (leaf 4)) 2 Empty) 1 (Node Empty 10 (leaf 15))
{-
    1
   / \
  2   10 
 /     \
6       15
 \
  4
-}

-- Basic methods

-- empty: empty tree is a tree without any node.
empty :: BinTree a -> Bool
empty Empty = True
empty _ = False

-- Elegant display

-- leaf: help a tree to be easily expressed
leaf :: a -> BinTree a
leaf k = Node Empty k Empty

-- goodShow: help a tree to be clearly shown
goodShow :: Show a => BinTree a -> String
goodShow Empty = "Empty"
goodShow (Node Empty k Empty) = "(Leaf " ++ show k ++ ")"
goodShow (Node l k r) = 
    "(Node " ++ goodShow l ++ " " ++ show k ++ " " ++ goodShow r ++ ")"

-- Attributes (Typeclasses)

instance Eq a => Eq (BinTree a) where
    -- (==) :: Eq a => BinTree a -> BinTree a -> Bool
    (==) Empty Empty = True
    (==) Empty (Node l k r) = False
    (==) (Node l k r) Empty = False
    (==) (Node l k r) (Node l' k' r') = l == l' && k == k' && r == r'

-- FAM

instance Functor BinTree where
    -- fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap f Empty = Empty
    fmap f (Node l k r) = Node (fmap f l) (f k) (fmap f r)

-- Structure

instance Foldable BinTree where
    -- foldMap :: Monoid m => (a -> m) -> BinTree a -> m
    foldMap f Empty = mempty
    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Traversable BinTree where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> BinTree a -> f (BinTree b)
    traverse f Empty = pure Empty
    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r

-- For quick check

-- sizedArbTestBinTree: generate random bintrees with different depth
sizedArbTestBinTree :: Arbitrary a => Int -> Gen (BinTree a)
sizedArbTestBinTree 0 = leaf <$> arbitrary
-- sizedArbTestBinTree 0 = do
--     k <- arbitrary
--     return (leaf k)
sizedArbTestBinTree n = do
    k <- arbitrary
    lDepth <- choose (0, n-1)
    l <- sizedArbTestBinTree lDepth
    rDepth <- choose (0, n-1)
    r <- sizedArbTestBinTree rDepth
    return $ Node l k r

-- quickerSizedArbTestBinTree: avoid the depth being too large
quickerSizedArbTestBinTree :: Arbitrary a => Int -> Gen (BinTree a)
quickerSizedArbTestBinTree n = sizedArbTestBinTree (min 10 n)

instance Arbitrary a => Arbitrary (BinTree a) where
    arbitrary = sized quickerSizedArbTestBinTree

-- Methods

-- depth: the level in which the deepest node lies
depth :: BinTree a -> Int
depth Empty = 0
depth (Node l k r) = succ $ max (depth l) (depth r)

-- cmpStruct: whether two trees are the same in structure
cmpStruct :: BinTree a -> BinTree b -> Bool
cmpStruct Empty Empty = True
cmpStruct Empty (Node l k r) = False
cmpStruct (Node l k r) Empty = False
cmpStruct (Node l k r) (Node l' _ r') = cmpStruct l l' && cmpStruct r r'