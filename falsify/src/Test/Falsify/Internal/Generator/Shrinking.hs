module Test.Falsify.Internal.Generator.Shrinking (
    -- * User-specified shrinking
    shrinkToOneOf
  , firstThen
  , shrinkWith
    -- * Support for shrink trees
  , fromShrinkTree
  , toShrinkTree
  , toShrinkTreeWithContext
  ) where

import Prelude hiding (properFraction)

import Data.Word

import qualified Data.Tree as Rose

import Test.Falsify.Internal.Generator
import Test.Falsify.SampleTree (SampleTree(..), Sample(..))
import Test.Falsify.ShrinkTree (ShrinkTree(..))

import qualified Test.Falsify.Context    as Context
import qualified Test.Falsify.ShrinkTree as ShrinkTree

{-------------------------------------------------------------------------------
  Specialized shrinking behaviour
-------------------------------------------------------------------------------}

-- | Start with @x@, then shrink to one of the @xs@
--
-- Once shrunk, will not shrink again.
--
-- Minimal value is the first shrunk value, if it exists, and the original
-- otherwise.
shrinkToOneOf :: forall a. a -> [a] -> Gen a
shrinkToOneOf x xs =
    aux <$> primWith shrinker
  where
    aux :: Sample -> a
    aux (NotShrunk _) = x
    aux (Shrunk    i) = index i xs

    -- When we shrink, we will try a bunch of new sample trees; we must ensure
    -- that we can try /any/ of the possible shrunk values.
    --
    -- We use this to implement 'fromShrinkTree'. Here, we explore a rose tree
    -- of possibilities; at every level in the tree, once we make a choice,
    -- we should commit to that choice and not consider it over and over again.
    -- Thus, once shrunk, we should not shrink any further.
    shrinker :: Sample -> [Word64]
    shrinker (Shrunk _)    = []
    shrinker (NotShrunk _) = zipWith const [0..] xs

    -- Index the list of possible shrunk values. This is a bit like @(!!)@ from
    -- the prelude, but with some edge cases.
    --
    -- - If the list is empty, we return the unshrunk value.
    -- - Otherwise, if the index exceeds the bounds, we return the last element.
    --
    -- These two special cases can arise in one of two circumstances:
    --
    -- - When we run the generator against the 'Minimal' tree. This will give us
    --   a @Shrunk 0@ value, independent of what the specified shrinking
    --   function does, and it is important that we produce the right value.
    -- - When the generator is run against a sample tree that was shrunk wrt to
    --   a /different/ generator. In this case the value could be anything;
    --   we return the final ("least preferred") element, and then rely on
    --   later shrinking to replace this with a more preferred element.
    index :: Word64 -> [a] -> a
    index _ []     = x
    index _ [y]    = y
    index 0 (y:_)  = y
    index n (_:ys) = index (n - 1) ys

-- | Generator that always produces @x@ as initial value, and shrinks to @y@
firstThen :: forall a. a -> a -> Gen a
firstThen x y = x `shrinkToOneOf` [y]

-- | Shrink with provided shrinker
--
-- This provides compatibility with QuickCheck-style manual shrinking.
--
-- Defined in terms of 'fromShrinkTree'; see discussion there for some
-- notes on performance.
shrinkWith :: forall a. (a -> [a]) -> Gen a -> Gen a
shrinkWith f gen = do
    -- It is critical that we do not apply normal shrinking of the 'SampleTree'
    -- here (not even to 'Minimal'). If we did, then the resulting shrink tree
    -- would change, and we would be unable to iteratively construct a path
    -- through the shrink tree.
    --
    -- Of course, it can still happen that the generator gets reapplied in a
    -- different context; we must take this case into account in
    -- 'shrinkToOneOf'.
    x <- withoutShrinking gen
    fromShrinkTree $ ShrinkTree.unfold x f

{-------------------------------------------------------------------------------
  Shrink trees
-------------------------------------------------------------------------------}

-- | Construct generator from shrink tree
--
-- This provides compatibility with Hedgehog-style integrated shrinking.
--
-- This is O(n^2) in the number of shrink steps: as this shrinks, the generator
-- is growing a path of indices which locates a particular value in the shrink
-- tree (resulting from unfolding the provided shrinking function). At each
-- step during the shrinking process the shrink tree is re-evaluated and the
-- next value in the tree is located; since this path throws linearly, the
-- overall cost is O(n^2).
--
-- The O(n^2) cost is only incurred on /locating/ the next element to be tested;
-- the property is not reevaluated at already-shrunk values.
fromShrinkTree :: forall a. ShrinkTree a -> Gen a
fromShrinkTree = go . unwrapShrinkTree
  where
    go :: Rose.Tree a -> Gen a
    go (Rose.Node x xs) = do
        next <- Nothing `shrinkToOneOf` map Just xs
        case next of
          Nothing -> return x
          Just x' -> go x'

-- | Expose the full shrink tree of a generator
--
-- The generator is passed 'Nothing' for the initial step, and then 'Just'
-- the number of the shrink step after that.
--
-- The 'Final' step is not /not/ included.
--
-- This generator does not shrink.
toShrinkTree :: forall a. Gen a -> Gen (ShrinkTree a)
toShrinkTree gen = fmap snd <$> toShrinkTreeWithContext False (const gen)

-- | Generalization of 'toShrinkTree'
toShrinkTreeWithContext :: forall a.
     Bool -- ^ Include 'Final' step?
  -> (Context.Execution -> Gen a)
  -> Gen (ShrinkTree (Context.Execution, a))
toShrinkTreeWithContext includeFinal gen = do
    initSeed <- Seed Context.Initial <$> captureLocalTree
    return $ WrapShrinkTree $ Rose.unfoldTree aux initSeed
  where
    aux :: Seed -> ((Context.Execution, a), [Seed])
    aux seed@Seed{seedContext, seedSampleTree} =
        case runGen (gen seedContext) seedSampleTree of
          (a, shrunk) -> ((seedContext, a), nextSeed seed shrunk)

    nextSeed :: Seed -> [SampleTree] -> [Seed]
    nextSeed Seed{seedContext, seedSampleTree} shrunk =
       case seedContext of
         Context.Initial ->
           case shrunk of
             [] | includeFinal -> [Seed (Context.Final 0) seedSampleTree]
             _  -> map (Seed $ Context.Shrinking 0) shrunk
         Context.Shrinking i ->
           case shrunk of
             [] | includeFinal -> [Seed (Context.Final $ succ i) seedSampleTree]
             _  -> map (Seed $ Context.Shrinking $ succ i) shrunk
         Context.Final _ ->
           []

-- | Internal: 'Rose.unfoldTree' seed, for constructing the shrink tree
data Seed = Seed{
      seedContext    :: Context.Execution
    , seedSampleTree :: SampleTree
    }
