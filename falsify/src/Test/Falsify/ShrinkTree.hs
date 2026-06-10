-- | Hedgehog-style shrink trees
--
-- Intended for qualified import.
--
-- > import Test.Falsify.ShrinkTree (ShrinkTree(..))
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

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Quickcheck-style manual shrinking
unfold :: a -> (a -> [a]) -> ShrinkTree a
unfold x f = WrapShrinkTree $ Rose.unfoldTree (\a -> (a, f a)) x
