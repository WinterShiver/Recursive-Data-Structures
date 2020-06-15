Haskell递归数据结构：二叉树01a-二叉树的基础定义

主要涉及二叉树的定义、性质（类型类）和QuickCheck。这篇文章是给后面的系列文章作铺垫的。相关代码在[BinTree.hs](https://github.com/WinterShiver/Recursive-Data-Structures/blob/master/BinTree.hs).

# 二叉树的递归定义

```haskell
data BinTree a = Empty | Node (BinTree a) a (BinTree a)
```
其中`Empty`是不包含任何结点的树，结点个数为0.下文将其记为“空树”。注意区分空树和“只有一个根节点的树”。

# 二叉树所处的类型类

二叉树具有递归的、能够容纳值的结构特征，属于`Eq, Functor, Foldable, Traversable`都很自然。

```haskell
instance Eq a => Eq (BinTree a) where
    -- (==) :: Eq a => BinTree a -> BinTree a -> Bool
    (==) Empty Empty = True
    (==) Empty (Node l k r) = False
    (==) (Node l k r) Empty = False
    (==) (Node l k r) (Node l' k' r') = l == l' && k == k' && r == r'

instance Functor BinTree where
    -- fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap f Empty = Empty
    fmap f (Node l k r) = Node (fmap f l) (f k) (fmap f r)

instance Foldable BinTree where
    -- foldMap :: Monoid m => (a -> m) -> BinTree a -> m
    foldMap f Empty = mempty
    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Traversable BinTree where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> BinTree a -> f (BinTree b)
    traverse f Empty = pure Empty
    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
```

# QuickCheck

为了给二叉树这个类型使用QuickCheck，需要额外做一些定义，使系统能随机生成深度不同的二叉树：

```haskell
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
```

这一部分比较浅显，`sizedArbTestBinTree n`随机生成最大深度为`n+1`的二叉树，从而能在QuickCheck中为待验证的东西提供测试用例。