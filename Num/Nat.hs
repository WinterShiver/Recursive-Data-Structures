{-# LANGUAGE InstanceSigs #-}

module Num.Nat where

-- Nat: natural numbers, Z is 0, S is succ
data Nat = Z | S Nat deriving (Show, Eq, Ord)

instance Enum Nat where
    toEnum :: Int -> Nat
    toEnum 0 = Z
    toEnum i = S $ toEnum $ pred i
    fromEnum :: Nat -> Int
    fromEnum Z = 0
    fromEnum (S n) = succ (fromEnum n)
    
-- Note: 
-- 1. Since Nat has infinite valid values, pred Z = \infty, and any
--    expression about pred Z could be dangerous.

-- Future work: 

-- 1. Nat, max and Z forms a semigroup. 

instance Semigroup Nat where
    (<>) = max

instance Monoid Nat where
    mempty = Z