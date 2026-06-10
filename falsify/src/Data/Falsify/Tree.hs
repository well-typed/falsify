-- | Binary trees
--
-- Intended for qualified import.
--
-- > import Data.Falsify.Tree (Tree(..))
-- > import qualified Data.Falsify.Tree as Tree
module Data.Falsify.Tree (
    Tree(Leaf, Branch)
    -- * Properties
  , size
  , weight
  , height
    -- * BST
  , lookup
    -- * Balancing
  , isWeightBalanced
  , isHeightBalanced
    -- * Debugging
  , render
  ) where

import Prelude hiding (lookup)

import GHC.Show

import qualified Data.Tree as Rose

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Binary tree
--
-- Each branch caches the size of the subtree, so that 'size' can be @O(1)@.
data Tree a =
    Leaf

    -- 'Branch_' caches the size of the tree
  | Branch_ {-# UNPACK #-} !Word a (Tree a) (Tree a)
  deriving stock (Eq, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Size of the tree
--
-- @O(1)@
size :: Tree a -> Word
size Leaf              = 0
size (Branch_ s _ _ _) = s

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
  Pattern synonyms that hide the size argument
-------------------------------------------------------------------------------}

viewBranch :: Tree a -> Maybe (a, Tree a, Tree a)
viewBranch Leaf              = Nothing
viewBranch (Branch_ _ x l r) = Just (x, l, r)

branch :: a -> Tree a -> Tree a -> Tree a
branch x l r = Branch_ (1 + size l + size r) x l r

pattern Branch :: a -> Tree a -> Tree a -> Tree a
pattern Branch x l r <- (viewBranch -> Just (x, l, r))
  where
    Branch = branch

{-# COMPLETE Leaf, Branch #-}

{-------------------------------------------------------------------------------
  'Show' instance that depends on the pattern synonyms
-------------------------------------------------------------------------------}

instance Show a => Show (Tree a) where
  showsPrec _ Leaf           = showString "Leaf"
  showsPrec a (Branch x l r) = showParen (a > appPrec) $
        showString "Branch "
      . showsPrec appPrec1 x
      . showSpace
      . showsPrec appPrec1 l
      . showSpace
      . showsPrec appPrec1 r

{-------------------------------------------------------------------------------
  BST
-------------------------------------------------------------------------------}

-- | Look value up in BST
--
-- NOTE: The 'Tree' datatype itself does /NOT/ guarantee that the tree is in
-- fact a BST. It is the responsibility of the caller to ensure this.
lookup :: Ord a => a -> Tree (a, b) -> Maybe b
lookup a' (Branch (a, b) l r)
  | a' < a    = lookup a' l
  | a' > a    = lookup a' r
  | otherwise = Just b
lookup _ Leaf = Nothing

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

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Render tree
--
-- This is intended for debugging only.
render :: Tree String -> String
render = Rose.drawTree . conv
  where
    conv :: Tree String -> Rose.Tree String
    conv Leaf           = Rose.Node "*" []
    conv (Branch x l r) = Rose.Node x [conv l, conv r]
