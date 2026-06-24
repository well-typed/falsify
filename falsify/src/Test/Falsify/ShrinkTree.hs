-- | Hedgehog-style shrink trees
--
-- Intended for qualified import.
--
-- > import Test.Falsify
-- > import qualified Test.Falsify.ShrinkTree as ShrinkTree
module Test.Falsify.ShrinkTree (
    ShrinkTree(..)
    -- * Construction
  , unfold
  ) where

import qualified Data.Tree as Rose

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Hedgehog-style shrink tree
newtype ShrinkTree a = WrapShrinkTree{
      unwrapShrinkTree :: Rose.Tree a
    }
  deriving stock (Eq, Functor)

instance Show a => Show (ShrinkTree a) where
  show = Rose.drawTree . fmap show . unwrapShrinkTree

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Quickcheck-style manual shrinking
unfold :: a -> (a -> [a]) -> ShrinkTree a
unfold x f = WrapShrinkTree $ Rose.unfoldTree (\a -> (a, f a)) x
