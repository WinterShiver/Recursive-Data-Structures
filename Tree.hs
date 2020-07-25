module Tree where

import Control.Applicative
import Test.QuickCheck

data Tree a = Node a [Tree a] deriving (Show)

testT1 = Node 2 [Node 3 [pure 4, pure 5], Node 8 [pure 9, Node 12 [pure 14, pure 16], Node 17 [pure 19]], pure 24, Node 28 [pure 31]]

instance Eq a => Eq (Tree a) where
    -- (==) :: Eq a => Tree a -> Tree a -> Bool
    (==) (Node k sts) (Node k' sts') = 
        k == k' && and (zipWith (==) sts sts')

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node k sts) = Node (f k) [fmap f st | st <- sts]

instance Applicative Tree where
    -- pure :: a -> Tree a 
    pure k = Node k []
    -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    (<*>) ft kt = Node (f k) (
        [fmap f st | st <- sts] ++ [fst <*> kt | fst <- fsts]
        ) where  
            ft = Node f fsts
            kt = Node k sts

instance Monad Tree where  
    -- (>>=) :: Tree a -> (a -> Tree b) -> Tree b
    (>>=) (Node k sts) f = Node k' (sts' ++ [st >>= f | st <- sts]) where  
        Node k' sts' = f k

-- For quick check

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree 0 = pure <$> arbitrary
arbitrarySizedTree m = do
    k <- arbitrary
    subTreeCount <- choose (0, 4)
    sts <- vectorOf subTreeCount (arbitrarySizedTree (m-1))
    return (Node k sts)

quickerArbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
quickerArbitrarySizedTree m = arbitrarySizedTree (min 10 m)    

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized quickerArbitrarySizedTree

-- Methods 

depth :: Tree a -> Int
depth (Node k []) = 1
depth (Node k sts) = succ $ max [depth st | st <- sts]
