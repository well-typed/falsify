-- | Utilities for working with trees with marked elements
--
-- Intended for qualified import.
--
-- > import qualified Test.Falsify.Internal.Marked.Tree as MarkedTree
module Test.Falsify.Internal.Marked.Tree (
    propagate
  , apply
  , keepAtLeast
  ) where

import Prelude hiding (drop)

import Control.Monad.State
import Control.Selective (Selective, ifS)

import Data.Falsify.Tree (Tree(..))
import Test.Falsify.Marked (Mark(..), Marked(..))

import qualified Test.Falsify.Marked as Marked
import qualified Data.Falsify.Tree   as Tree

{-------------------------------------------------------------------------------
  Utilities for working with marked trees
-------------------------------------------------------------------------------}

-- | Propagate 'Drop' marker down the tree
--
-- This is useful in conjunction with 'apply', which truncates entire subtrees.
propagate :: Tree (Marked f a) -> Tree (Marked f a)
propagate = keep
  where
    keep :: Tree (Marked f a) -> Tree (Marked f a)
    keep Leaf                         = Leaf
    keep (Branch (Marked Keep x) l r) = Branch (Marked Keep x) (keep l) (keep r)
    keep (Branch (Marked Drop x) l r) = Branch (Marked Drop x) (drop l) (drop r)

    drop :: Tree (Marked f a) -> Tree (Marked f a)
    drop = fmap $ \(Marked _ x) -> Marked Drop x

-- | Generate those values we want to keep
--
-- Whenever we meet an element marked 'Drop', that entire subtree is dropped.
apply :: forall f a. Selective f => Tree (Marked f a) -> f (Tree a)
apply = go
  where
    go :: Tree (Marked f a) -> f (Tree a)
    go Leaf                      = pure Leaf
    go (Branch (Marked m g) l r) = ifS (pure $ m == Keep)
                                     (Branch <$> g <*> go l <*> go r)
                                     (pure Leaf)

-- | Change enough nodes currently marked as 'Drop' to 'Keep' to ensure at
-- least @n@ nodes are marked 'Keep'.
--
-- Precondition: any 'Drop' marks must have been propagated; see 'propagate'.
-- Postcondition: this property is preserved.
keepAtLeast :: Word -> Tree (Marked f a) -> Tree (Marked f a)
keepAtLeast = \n t ->
    let kept = Marked.countKept t
    in if kept >= n
         then t
         else evalState (go t) (n - kept)
  where
    go :: Tree (Marked f a) -> State Word (Tree (Marked f a))
    go   Leaf                         = return Leaf
    go   (Branch (Marked Keep x) l r) = Branch (Marked Keep x) <$> go l <*> go r
    go t@(Branch (Marked Drop x) l r) = get >>= \case
         0 ->
           -- Nothing left to drop
           return t
         n | Tree.size t <= n -> do
          -- We can keep the entire subtree
          put $ n - Tree.size t
          return $ fmap (Marked Keep . unmark) t
         n ->  do
          -- We cannot delete the entire subtree. In order to preserve the
          -- "drop property", we /must/ mark this node as 'Keep'
          put $ n - 1
          Branch (Marked Keep x) <$> go l <*> go r
