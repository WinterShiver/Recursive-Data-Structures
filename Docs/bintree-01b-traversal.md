Haskell递归数据结构：二叉树01b-二叉树的前序、中序、后序、层序遍历

阅读本文前建议首先阅读二叉树01a，了解二叉树的定义。本文主要涉及二叉树的前、中、后、层序遍历。相关代码在[BinTree.Traversal](https://github.com/WinterShiver/Recursive-Data-Structures/blob/master/BinTree/Traversal.hs).

前序遍历：根结点的值 + 左子树前序遍历 + 右子树前序遍历，获得的结点值序列。中序、后序类似。
层序遍历：从上到下，同一层从左向右地遍历，获得的结点值序列。

前中后序遍历都属于DFS，简单递归即可。层序遍历需要用到一个尾递归起到状态传递的作用，执行BFS，具体解读可见[Haskell：实现二叉树及其前序、中序、后序遍历和层序遍历](https://blog.csdn.net/WinterShiver/article/details/103658960)。