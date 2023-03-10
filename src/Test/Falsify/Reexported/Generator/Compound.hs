-- | Compound generators
module Test.Falsify.Reexported.Generator.Compound (
    -- * Lists
    list
  , elem
    -- * Trees
    -- ** Binary trees
  , tree
  , bst
    -- ** Rose trees
  , RoseTree
  , path
    -- * Auxiliary
  , shrinkToNothing
  , mark
  ) where

import Prelude hiding (either, elem)

import Control.Monad
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)

import qualified Data.List.NonEmpty as NE
import qualified Data.Tree          as Rose

import Data.Falsify.Marked (Marked(..))
import Data.Falsify.Tree (Tree(..), Interval(..), Endpoint(..))
import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Range (Range)
import Test.Falsify.Reexported.Generator.Instances
import Test.Falsify.Reexported.Generator.Simple

import qualified Data.Falsify.Marked as Marked
import qualified Data.Falsify.Tree   as Tree
import qualified Test.Falsify.Range  as Range

{-------------------------------------------------------------------------------
  Auxiliary: marking elements
-------------------------------------------------------------------------------}

-- | Start with @Just x@ for some @x@, then shrink to @Nothing@
shrinkToNothing :: Gen a -> Gen (Maybe a)
shrinkToNothing g = firstThen Just (const Nothing) <*> g

-- | Mark an element, shrinking towards 'Drop'
--
-- This is similar to 'shrinkToNothing', except that 'Marked' still has a value in the
-- 'Drop' case: marks are merely hints, that we may or may not use (e.g., see
-- 'Marked.keepAtLeast').
mark :: Gen a -> Gen (Marked a)
mark g = firstThen Keep Drop <*> g

{-------------------------------------------------------------------------------
  Lists
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

-- | Choose random element
--
-- Shrinks towards earlier elements.
elem :: NonEmpty a -> Gen a
elem xs = (toList xs !!) <$> integral (Range.between (0, length xs - 1))

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

-- | Construct binary search tree
--
-- Shrinks by replacing entire subtrees by the empty tree.
bst :: forall a b. Integral a => (a -> Gen b) -> Interval a -> Gen (Tree (a, b))
bst gen = go >=> traverse (\a -> (a,) <$> gen a)
  where
    go :: Interval a -> Gen (Tree a)
    go i =
        case Tree.inclusiveBounds i of
          Nothing       -> pure Leaf
          Just (lo, hi) -> firstThen id (const Leaf) <*> go' lo hi

    -- inclusive bounds, lo <= hi
    go' :: a -> a -> Gen (Tree a)
    go' lo hi = Branch mid
            <$> go (Interval (Inclusive lo) (Exclusive mid))
            <*> go (Interval (Exclusive mid) (Inclusive hi))
      where
        mid :: a
        mid = lo + ((hi - lo) `div` 2)

{-------------------------------------------------------------------------------
  Rose trees
-------------------------------------------------------------------------------}

type RoseTree = Rose.Tree

-- | Generate random path through the tree
--
-- Shrinks towards shorter paths, and towards paths that use subtrees that
-- appear earlier in the list of subtrees at any node in the tree.
path :: RoseTree a -> Gen (NonEmpty a)
path (Rose.Node x [])     = pure (x :| [])
path (Rose.Node x (y:ys)) = pure (x :| [])
                       <!> (elem (y :| ys) >>= fmap (NE.cons x) . path)
