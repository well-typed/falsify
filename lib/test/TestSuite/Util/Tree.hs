module TestSuite.Util.Tree (
    -- * Stats
    size
  , weight
  , height
    -- * Balancing
  , isWeightBalanced
  , isHeightBalanced
  ) where

import Test.Falsify.Generator (Tree(..))

{-------------------------------------------------------------------------------
  Tree stats
-------------------------------------------------------------------------------}

-- | Size of the tree
size :: Tree a -> Word
size Leaf           = 0
size (Branch _ l r) = 1 + size l + size r

-- | Weight of the tree
--
-- The weight of a tree is simply its size plus one.
--
-- @O(1)@
weight :: Tree a -> Word
weight = succ . size

-- | Height of the tree
--
-- The height of a tree is the maximum length from the root to any of the leafs.
--
-- @O(1)@
height :: Tree a -> Word
height Leaf           = 0
height (Branch _ l r) = 1 + max (height l) (height r)

{-------------------------------------------------------------------------------
  Balancing
-------------------------------------------------------------------------------}

-- | Check if the tree is weight-balanced
--
-- A tree is weight-balanced if the weights of the subtrees does not differ
-- by more than a factor 3.
--
-- See "Balancing weight-balanced trees", Hirai and Yamamoto, JFP 21(3), 2011.
isWeightBalanced :: Tree a -> Bool
isWeightBalanced = checkBalanceCondition isBalanced
  where
    delta :: Word
    delta = 3

    isBalanced :: Tree a -> Tree a -> Bool
    isBalanced a b = and [
          delta * weight a >= weight b
        , delta * weight b >= weight a
        ]

-- | Check if a tree is height-balanced
--
-- A tree is height balanced if the heights of its subtrees do not differ
-- by more than one.
isHeightBalanced :: Tree a -> Bool
isHeightBalanced = checkBalanceCondition isBalanced
  where
    isBalanced :: Tree a -> Tree a -> Bool
    isBalanced a b = or [
          (height a <= height b) && (height b - height a <= 1)
        , (height b <= height a) && (height a - height b <= 1)
        ]

-- | Internal auxiliary: check given tree balance condition
--
-- Property @p l r@ will be checked at every branch in the tree.
checkBalanceCondition :: forall a. (Tree a -> Tree a -> Bool) -> Tree a -> Bool
checkBalanceCondition p = go
  where
    go :: Tree a -> Bool
    go Leaf           = True
    go (Branch _ l r) = and [p l r, go l, go r]

