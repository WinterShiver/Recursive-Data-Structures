Haskell递归数据结构：二叉树01b-二叉树的前序、中序、后序、层序遍历

阅读本文前建议首先阅读二叉树01a，了解二叉树的定义。本文主要涉及二叉树的前、中、后、层序遍历。相关代码在[BinTree.Traversal](https://github.com/WinterShiver/Recursive-Data-Structures/blob/master/BinTree/Traversal.hs).

前序遍历的概念：根结点的值 + 左子树前序遍历 + 右子树前序遍历，获得的结点值序列。中序、后序类似。前中后序遍历的实现：通过DFS实现，简单递归即可。

层序遍历的概念：从上到下，同一层从左向右地遍历，获得的结点值序列。层序遍历的实现：用一个尾递归起到状态传递的作用，保存并更新BFS队列。

```haskell
levelOrderTraversal :: BinTree a -> [a]
levelOrderTraversal tree = step ([], [tree]) where 
    step (result, trees) = case trees of
        [] -> result
        Empty:ts -> step (result, ts)
        (Node l k r):ts -> step (result++[k], ts++[l, r])
```
