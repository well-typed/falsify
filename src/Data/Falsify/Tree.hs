module Data.Falsify.Tree (
    Tree(Leaf, Branch)
    -- * Stats
  , size
  , weight
  , height
    -- * Balancing
  , isWeightBalanced
  , isHeightBalanced
    -- * Dealing with marks
  , propagate
  , truncate
  , keepAtLeast
    -- * Debugging
  , draw
  ) where

import Prelude hiding (drop, truncate)

import Control.Monad.State
import GHC.Show

import Data.Falsify.Marked (Marked(..))

import qualified Data.Falsify.Marked as Marked
import qualified Data.Tree as Containers

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Tree a =
    Leaf
  | Branch_ {-# UNPACK #-} !Stats a (Tree a) (Tree a)
  deriving stock (Eq, Functor, Foldable, Traversable)

data Stats = Stats {
      size_   :: {-# UNPACK #-} !Word
    , height_ :: {-# UNPACK #-} !Word
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Tree stats
-------------------------------------------------------------------------------}

-- | Size of the tree
--
-- @O(1)@
size :: Tree a -> Word
size Leaf              = 0
size (Branch_ s _ _ _) = size_ s

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
height Leaf              = 0
height (Branch_ s _ _ _) = height_ s

{-------------------------------------------------------------------------------
  Pattern synonyms that hide the size argument
-------------------------------------------------------------------------------}

viewBranch :: Tree a -> Maybe (a, Tree a, Tree a)
viewBranch Leaf              = Nothing
viewBranch (Branch_ _ x l r) = Just (x, l, r)

branch :: a -> Tree a -> Tree a -> Tree a
branch x l r = Branch_ s x l r
  where
    s :: Stats
    s = Stats {
          size_   = 1 + (size   l   +   size   r)
        , height_ = 1 + (height l `max` height r)
        }

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
          height a <= height b && height b - height a <= 1
        , height b <= height a && height b - height a <= 1
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
  Dealing with marks
-------------------------------------------------------------------------------}

-- | Propagate 'Drop' marker down the tree
--
-- This is useful in conjunction with 'truncate', which truncates entire
-- subtrees.
propagate :: Tree (Marked a) -> Tree (Marked a)
propagate = keep
  where
    keep :: Tree (Marked a) -> Tree (Marked a)
    keep Leaf                  = Leaf
    keep (Branch (Keep x) l r) = Branch (Keep x) (keep l) (keep r)
    keep (Branch (Drop x) l r) = Branch (Drop x) (drop l) (drop r)

    drop :: Tree (Marked a) -> Tree (Marked a)
    drop = fmap (Drop . Marked.forget)

-- | Truncate tree
--
-- Whenever we meet an element marked 'Drop', that entire subtree is dropped.
truncate :: Tree (Marked a) -> Tree a
truncate = go
  where
    go :: Tree (Marked a) -> Tree a
    go Leaf                  = Leaf
    go (Branch (Drop _) _ _) = Leaf
    go (Branch (Keep x) l r) = Branch x (go l) (go r)

-- | Change enough nodes currently marked as 'Drop' to 'Keep' to ensure at
-- least @n@ nodes are marked 'Keep'.
--
-- This function is different from 'Data.Falsify.Marked.keepAtLeast':
--
-- * It /requires/ as precondition that any 'Drop' marks must have been
--   propagated; see 'propagate'.
-- * It /guarantees/ as postcondition that this property is preserved.
keepAtLeast :: Word -> Tree (Marked a) -> Tree (Marked a)
keepAtLeast = \n t ->
    let kept = Marked.countKept t
    in if kept >= n
         then t
         else evalState (go t) (n - kept)
  where
    go :: Tree (Marked a) -> State Word (Tree (Marked a))
    go   Leaf                  = return Leaf
    go   (Branch (Keep x) l r) = Branch (Keep x) <$> go l <*> go r
    go t@(Branch (Drop x) l r) = get >>= \case
         0 ->
           -- Nothing left to drop
           return t
         n | size t <= n -> do
          -- We can delete the entire subtree
          put $ n - size t
          return $ fmap (Keep . Marked.forget) t
         n ->  do
          -- We cannot delete the entire subtree. In order to preserve the
          -- "drop property", we /must/ mark this node as 'Keep'
          put $ n - 1
          Branch (Keep x) <$> go l <*> go r

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

draw :: Tree String -> String
draw = Containers.drawTree . conv
  where
    conv :: Tree String -> Containers.Tree String
    conv Leaf           = Containers.Node "*" []
    conv (Branch x l r) = Containers.Node x [conv l, conv r]
