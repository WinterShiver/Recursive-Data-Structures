Haskell递归数据结构：二叉树02a-二叉树和层序遍历列表之间的转换（上）

本文主要涉及完全二叉树、完美二叉树的概念和递归判定，为下一篇文章：二叉树和层序遍历列表之间的互相转换进行铺垫。相关代码在[BinTree.Complete](https://github.com/WinterShiver/Recursive-Data-Structures/blob/master/BinTree/Complete.hs).

# 预备知识

首先，阅读本文前建议首先阅读：
- 二叉树01a：二叉树的基本定义和性质。
- 二叉树01b：层序遍历的代码和理解。

## 空树的判定

判断二叉树是否为空树的函数`empty`，这个函数定义在[BinTree.hs](https://github.com/WinterShiver/Recursive-Data-Structures/blob/master/BinTree.hs)：

```haskell
empty :: BinTree a -> Bool
empty Empty = True
empty _ = False
```

## 二叉树的深度，及其递归定义

结点的层次：规定根结点在1层，其它任一结点的层数是其父结点的层数加1。
二叉树的深度：树中所有结点中的最大层数是这棵树的深度。

求二叉树的深度用`depth`实现：简单递归即可。定义在[BinTree.hs](https://github.com/WinterShiver/Recursive-Data-Structures/blob/master/BinTree.hs).

```haskell
depth :: BinTree a -> Int
depth Empty = 0
depth (Node l k r) = succ $ max (depth l) (depth r)
```

`depth`的时间复杂度是`O(n)`，`n`是结点个数.

空树的深度记为0.这里为了方便，返回值类型使用了整数类型（这里是`Int`），而不是自然数类型（比如`Nat`）。但我们要注意到这个函数的返回值始终是不小于0的，始终是一个自然数。

注意到一个浅显的事实：`plus = \l -> \r -> succ $ max (depth l) (depth r)`不满足结合律，因此在自然数集上的`plus`二元运算无法构成半群，从而不能用`foldMap`来简化这个递归。这个判定半群的题曾经出现在我的离散数学教材上，当时没想到这个题的现实背景是这样的。（？无端联想）

# 完全二叉树、完美二叉树

这一部分涉及的代码在[BinTree.Complete](https://github.com/WinterShiver/Recursive-Data-Structures/blob/master/BinTree/Complete.hs).

## 定义

对满二叉树、完全二叉树、完美二叉树的标准定义，我们采用[Binary tree - Wikipedia](en.wikipedia.org/wiki/Binary_tree)的说法。简单解释就是：

- 完全二叉树(complete bianry tree)就是结点从上到下、从左到右地填充，除最后一层之外都填满，而最后一层从左侧开始填充。
- 完美二叉树(perfect bianry tree)就是每一层都被结点填满。

另外在[BinTree.Complete](https://github.com/WinterShiver/Recursive-Data-Structures/blob/master/BinTree/Complete.hs)还定义了满二叉树(full binary tree)：二叉树中的结点要么是叶子结点，要么拥有两个子结点，不会只有一个子结点。但这个概念与本文无关，这里就不多展开了。这里额外多说一句：很多国内教材的“满二叉树”对应这篇文章的perfect bianry tree，而不是full binary tree，注意做好区分。

本文中，定义空树是满二叉树、完全二叉树、完美二叉树。只有一个结点的树也是满二叉树、完全二叉树、完美二叉树，但是它被定义覆盖到了，并不需要特殊处理。

## 完美二叉树的递归判定

在递归结构的二叉树上，对complete和perfect的判定也是递归的。我们先从看起来比较简单的完美二叉树开始。

```haskell
perfect :: BinTree a -> Bool
perfect Empty = True
perfect (Node l k r) = depth l == depth r && perfect l && perfect r
```

树是perfect，当且仅当左右子树都perfect，而且高度相同。这个定义效率是`O(nlogn)`，`n`是结点个数，时间效率好像不太理想，但我暂时还没有查到或想到更好的办法，欢迎各种方式赐教。

证明这个算法的正确性时，可以先通过归纳法证明n层完美二叉树的结点个数为`2^n-1`，然后说明结点个数为`2^n-1`和完美二叉树定义的等价性。

## 完全二叉树的递归判定

设计complete的判定的时候，我们从完全二叉树插入元素的角度来想：
1. 首先是一棵perfect，此时左右子树是深度相同的perfect，下一个结点插入底层的最左侧；
2. 在底层从左到右插入结点，插入结点个数少于一半时，左侧变成深一层的complete，右子树依然是perfect；
3. 在插入一半时，左侧底层插满深一层的perfect，右侧是perfect；
4. 继续插入，左右深度重新相同，左侧perfect右侧complete；
5. 底层完全插满，左右都变成深度相同的perfect，下一个结点插入下一层的最左侧。

在这个过程中，只有2种情况：左比右高1，或者左右等高。左高时，树complete当且仅当左complete右perfect；等高时，树complete当且仅当左perfect右complete. 函数定义遂成。这个定义效率是`O(nlogn)`，`n`是结点个数。

```haskell
complete :: BinTree a -> Bool
complete Empty = True
complete (Node l k r) 
    | depth l == depth r = perfect l && complete r
    | depth l == succ (depth r) = complete l && perfect r
    | otherwise = False
```

## 性质的进一步说明

我们注意这段代码：

```haskell
type Cbt = BinTree -- bintree + complete
type Pbt = Cbt -- bintree + prefect
```

这段代码的意思是，因为perfect的二叉树必定complete，所以`type Pbt`套在`Cbt`上，说明以后的函数中，接受输入类型为`Cbt a`时，可以往里传一个`Pbt a`的值，反之则不行。这件事在下一篇文章中很重要。