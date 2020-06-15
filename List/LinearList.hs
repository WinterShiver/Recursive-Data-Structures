-- {-# LANGUAGE FlexibleInstances #-} -- to permit Num a as a Semigroup
-- {-# LANGUAGE UndecidableInstances #-} -- to permit Num a as a Semigroup

module List.LinearList where

-- List Definition

data List a = Nil | Cons a (List a) deriving (Show)

instance Foldable List where
    -- foldMap :: Monoid m => (a -> m) -> List a -> m
    foldMap f Nil = mempty
    foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Functor List where
    -- fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
    
instance Traversable List where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> List a -> f (List b)
    traverse f Nil = pure Nil
    traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

testList = Cons 1 $ Cons 20 $ Cons 4 $ Cons 3 $ Cons 50 Nil

-- Methods

empty :: List a -> Bool
empty Nil = True
empty _ = False


