-- | Compound generators
module Test.Falsify.Reexported.Generator.Compound (
    -- * Auxiliary
    shrinkToNothing
  , mark
    -- * Standard compound generators
  , either
  , list
    -- * Trees
  , tree
  , bst
  ) where

import Prelude hiding (either)

import Control.Monad
import Control.Selective
import Data.Maybe (mapMaybe)

import Data.Falsify.Marked (Marked(..))
import Data.Falsify.Tree (Tree(..), Interval(..), Endpoint(..))
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
  Compound generators
-------------------------------------------------------------------------------}

-- | Generate a value with one of two generators
--
-- Shrinks towards the first ('Left') generator.
--
-- In the remainder of this docstring we give some background to this function,
-- which may be useful for general understanding of the @falsify@ library.
--
-- The implementation of @either l r@ takes advantage of the that 'Gen' is a
-- selective functor to ensure that the two generators can shrink independently:
-- if the initial value of the generator is @Right y@ for some @y@, later shrunk
-- to @Right y'@, then if the generator can shrink to @Left x@ at some point,
-- shrinking effectively "starts over": the value of @y@ is independent of @x'@.
--
-- That is different from doing this:
--
-- > do b <- bool
-- >    if b then l else r
--
-- In this case, @l@ and @r@ will be generated from the /same/ sample tree,
-- and so cannot shrink independently.
--
-- It is /also/ different from
--
-- > do x <- l
-- >    y <- r
-- >    b <- bool
-- >    return $ if b then x else y
--
-- In this case, @l@ and @r@ are run against /different/ sample trees, like in
-- @either l r@, /but/ in this case if the current value produced by the
-- generator is @Right y@ for some @y@, then @x@ will always be shrunk to the
-- minimal value that @l@ can produce: this /must/ be possible because we're not
-- currently using @x@!
--
-- To rephrase that last point: generating values that are not actually used
-- will lead to poor shrinking, since those values can always be shrunk to their
-- minimal value, independently from whatever property is being tested: the
-- shrinker does not know that the value is not being used. The correct way to
-- conditionally use a value is to use the selective interface, as 'either'
-- does.
either :: Gen a -> Gen b -> Gen (Either a b)
either = eitherS (bool True)

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

-- | Construct binary search tree
--
-- Shrinks by replacing entire subtrees by the empty tree.
bst :: forall a b. Integral a => (a -> Gen b) -> Interval a -> Gen (Tree (a, b))
bst gen = go
  where
    go :: Interval a -> Gen (Tree (a, b))
    go i =
        case Tree.inclusiveBounds i of
          Nothing       -> pure Leaf
          Just (lo, hi) -> firstThen id (const Leaf) <*> go' lo hi

    -- inclusive bounds, lo <= hi
    go' :: a -> a -> Gen (Tree (a, b))
    go' lo hi = (\b -> Branch (mid, b))
            <$> gen mid
            <*> go (Interval (Inclusive lo) (Exclusive mid))
            <*> go (Interval (Exclusive mid) (Inclusive hi))
      where
        mid :: a
        mid = lo + ((hi - lo) `div` 2)

{-------------------------------------------------------------------------------
  Auxiliary: 'Selective'
-------------------------------------------------------------------------------}

-- | A typed version of 'ifS'
eitherS :: Selective f => f Bool -> f a -> f b -> f (Either a b)
eitherS b t f = ifS b (Left <$> t) (Right <$> f)