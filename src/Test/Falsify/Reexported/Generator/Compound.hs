-- | Compound generators
module Test.Falsify.Reexported.Generator.Compound (
    -- * Taking advantage of 'Selective'
    choose
    -- * Lists
  , list
  , elem
    -- ** Shuffling
  , shuffle
  , permutation
    -- * Trees
    -- ** Binary trees
  , tree
  , bst
    -- ** Shrink trees
  , IsValidShrink(..)
  , ShrinkTree
  , path
  , pathAny
    -- * Auxiliary
  , shrinkToNothing
  , mark
  ) where

import Prelude hiding (either, elem)

import Control.Monad
import Control.Selective
import Data.Either (either)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import Data.Void

import qualified Data.List.NonEmpty as NE
import qualified Data.Tree          as Rose

import Data.Falsify.List (Permutation, applyPermutation)
import Data.Falsify.Marked (Marked(..))
import Data.Falsify.Tree (Tree(..), Interval(..), Endpoint(..))
import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Generator.Shrinking (IsValidShrink(..))
import Test.Falsify.Range (Range)
import Test.Falsify.Reexported.Generator.Simple

import qualified Data.Falsify.Marked as Marked
import qualified Data.Falsify.Tree   as Tree
import qualified Test.Falsify.Range  as Range

{-------------------------------------------------------------------------------
  Taking advantage of 'Selective'
-------------------------------------------------------------------------------}

-- | Generate a value with one of two generators
--
-- Shrinks towards the first generator.
--
-- In the remainder of this docstring we give some background to this function,
-- which may be useful for general understanding of the @falsify@ library.
--
-- The implementation takes advantage of the that 'Gen' is a selective functor
-- to ensure that the two generators can shrink independently: if the initial
-- value of the generator is some @y@ produced by the second generator, later
-- shrunk to some @y'@, then if the generator can shrink to @x@ at some point,
-- produced by the /first/ generator, then shrinking effectively "starts over":
-- the value of @x@ is independent of @y'@.
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
-- In this case, @l@ and @r@ are run against /different/ sample trees, like we
-- do here, /but/ in this case if the current value produced by the generator is
-- produced by the right generator, then the sample tree used for the left
-- generator will always shrink to 'Minimal' (this /must/ be possible because
-- we're not currently using it); this means that we would then only be able to
-- shrink to a value from the left generator if the /minimal/ value produced by
-- that generator happens to work.
--
-- To rephrase that last point: generating values that are not actually used
-- will lead to poor shrinking, since those values can always be shrunk to their
-- minimal value, independently from whatever property is being tested: the
-- shrinker does not know that the value is not being used. The correct way to
-- conditionally use a value is to use the selective interface, as we do here.
choose :: Gen a -> Gen a -> Gen a
choose = ifS (bool True)

{-------------------------------------------------------------------------------
  Auxiliary: marking elements
-------------------------------------------------------------------------------}

-- | Start with @Just x@ for some @x@, then shrink to @Nothing@
shrinkToNothing :: Gen a -> Gen (Maybe a)
shrinkToNothing g = firstThen Just (const Nothing) <*> g

-- | Mark an element, shrinking towards 'Drop'
--
-- This is similar to 'shrinkToNothing', except that 'Marked' still has a value
-- in the 'Drop' case: marks are merely hints, that we may or may not use (e.g.,
-- see 'Marked.keepAtLeast').
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
  Shuffling
-------------------------------------------------------------------------------}

-- | Shuffle list (construct a permutation)
--
-- Shrinking behaviour: 'shuffle' is defined in terms of 'permutation', which
-- provides some guarantees: it shrinks towards making changes near the /start/
-- of the list, and towards swapping /fewer/ elements of the list.
--
-- It is difficult to define precisely how this affects the resulting list, but
-- we /can/ say that if for a particular counter-example it suffices if two
-- lists are different in /one/ element, then the shuffled list will in fact
-- only be different in /one/ place from the original, and that one element will
-- have been swapped with an immediate neighbour.
shuffle :: [a] -> Gen [a]
shuffle xs =
    flip applyPermutation xs <$>
      permutation (fromIntegral $ length xs)

-- | Generate permutation for a list of length @n@
--
-- This is essentially an implemention of Fisher-Yates, in that we generate a
-- series of swaps (i, j), with 1 <= i <= n - 1 and @0 <= j <= i@, except that
--
-- * We can shrink a choice of @i@ (towards 1).
-- * We can drop arbitrary swaps.
--
-- This ensures that we shrink towards making swaps nearer the /start/ of the
-- list, as well as towards /fewer/ swaps.
--
-- We make no attempt to make the permutation canonical; doing so makes it
-- extremely difficult to get predicable shrinking behaviour.
permutation :: Word -> Gen Permutation
permutation 0 = return []
permutation 1 = return []
permutation n =
    mapMaybe Marked.shouldKeep <$>
      traverse (mark . genSwap) [n - 1, n - 2 .. 1]
  where
    genSwap :: Word -> Gen (Word, Word)
    genSwap i = do
        i' <- integral $ Range.between (1, i)
        j  <- integral $ Range.between (i, 0)
        return (i', min i' j)

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
        inLeft <- integral $ Range.withOrigin (0, n - 1) ((n - 1) `div` 2)
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
  Shrink trees
-------------------------------------------------------------------------------}

type ShrinkTree = Rose.Tree

-- | Generate semi-random path through the tree
--
-- Will only construct paths that satisfy the given predicate (typically, a
-- property that is being tested).
--
-- Shrinks towards shorter paths, and towards paths that use subtrees that
-- appear earlier in the list of subtrees at any node in the tree.
--
-- See also 'pathAny'.
path :: forall a p n.
     (a -> IsValidShrink p n) -- ^ Predicate
  -> ShrinkTree a
  -> Gen (Either n (NonEmpty p))
path validShrink = \(Rose.Node a as) ->
    case validShrink a of
      InvalidShrink n -> pure $ Left n
      ValidShrink   p -> Right <$> go p as
  where
    go :: p -> [Rose.Tree a] -> Gen (NonEmpty p)
    go b as =
        case mapMaybe checkPred as of
          []   -> pure (b :| [])
          m:ms -> choose
                    (pure (b :| []))
                    (elem (m :| ms) >>= \(b', as') -> NE.cons b <$> go b' as')

    checkPred :: Rose.Tree a -> Maybe (p, [Rose.Tree a])
    checkPred (Rose.Node a as) =
       case validShrink a of
         InvalidShrink _ -> Nothing
         ValidShrink   b -> Just (b, as)

-- | Variation on 'path' without a predicate.
pathAny :: ShrinkTree a -> Gen (NonEmpty a)
pathAny = fmap (either absurd id) . path ValidShrink
