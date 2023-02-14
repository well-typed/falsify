-- | Compound generators
module Test.Falsify.Reexported.Generator.Compound (
    -- * Compound generators
    list
  , tree
  ) where

import Control.Monad
import Data.Maybe (mapMaybe)

import Data.Falsify.Marked (Marked(..))
import Data.Falsify.Tree (Tree(..))
import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Range (Range)
import Test.Falsify.Reexported.Generator.Simple

import qualified Data.Falsify.Marked as Marked
import qualified Data.Falsify.Tree   as Tree
import qualified Test.Falsify.Range  as Range

{-------------------------------------------------------------------------------
  Auxiliary: marking elements
-------------------------------------------------------------------------------}

-- | Mark an element, shrinking towards 'Drop'
--
-- This is an ingredient in many of the compound generators.
mark :: Gen a -> Gen (Marked a)
mark g = firstThen Keep Drop <*> g

{-------------------------------------------------------------------------------
  Compound generators


  An approach where we generate more elements than we need and then selecting
  some of them (possibly increasing how many we select as we "shrink") does NOT
  work: in such an approach, any value we generate but don't look at can
  trivially shrink to zero ('Minimal'); this means that if we then /do/ want to
  look at it later (because some other value would have been shrunk), we might
  only find zeroes: we want to start shrinking that only /after/ we start
  looking at it, otherwise shrinking means nothing.
-------------------------------------------------------------------------------}

-- | Generate list of specified length
list :: Range Word -> Gen a -> Gen [a]
list len gen = do
    -- We do /NOT/ mark this call to 'integral' as 'withoutShrinking': it could
    -- shrink towards larger values, in which case we really need to generate
    -- more elements. This doesn't really have any downsides: it merely means
    -- that we would prefer to shrink towards a prefix of the list first, before
    -- we try to drop random other elements from the list.
    --
    -- If we have an expression such as @(,) <$> list .. <*> list@, the two
    -- lists will be shrunk independently from each other due to the branching
    -- point above them. Hence, it doesn't matter if first generator uses "fewer
    -- samples" as it shrinks.
    --
    -- After we have a list of @n@ elements, we can then drop arbitrary elements
    -- from that list, but of course doing so well only /decrease/ the list
    -- length: if @Range.origin len > n@, then we should not drop anything (in
    -- that case, the only way we can get more elements is by "shrinking" @n@
    -- towards a larger number).
    n <- integral len
    mapMaybe Marked.shouldKeep . Marked.keepAtLeast (Range.origin len) <$>
      replicateM (fromIntegral n) (mark gen)

{-------------------------------------------------------------------------------
  Binary trees
-------------------------------------------------------------------------------}

-- | Generate binary tree
tree :: forall a. Range Word -> Gen a -> Gen (Tree a)
tree size gen = do
    n <- integral size
    Tree.truncate . Tree.keepAtLeast (Range.origin size) . Tree.propagate <$>
      go n
  where
    go :: Word -> Gen (Tree (Marked a))
    go 0 = return Leaf
    go n = do
        -- Generate element at the root
        x <- mark gen

        -- Choose how many elements to put in the left subtree
        --
        -- This ranges from none (right-biased) to all (left-biased), shrinking
        -- towards half the number of elements: hence, towards a balanced tree.
        inLeft <- integral $ Range.num (0, n - 1) ((n - 1) `div` 2)
        let inRight = (n - 1) - inLeft
        Branch x <$> go inLeft <*> go inRight
