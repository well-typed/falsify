-- | Compound generators
module Test.Falsify.Reexported.Generator.Compound (
    -- * Taking advantage of 'Selective'
    choose
  , oneof
    -- * Lists
  , list
  , elem
  , pick
  , pickBiased
    -- ** Shuffling
  , shuffle
  , permutation
    -- * Tweak test data distribution
  , frequency
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
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes)
import Data.Void

import qualified Data.List.NonEmpty as NE
import qualified Data.Tree          as Rose

import Data.Falsify.List (Permutation)
import Data.Falsify.Marked
import Data.Falsify.Tree (Tree(..), Interval(..), Endpoint(..))
import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Generator.Shrinking (IsValidShrink(..))
import Test.Falsify.Range (Range)
import Test.Falsify.Reexported.Generator.Shrinking
import Test.Falsify.Reexported.Generator.Simple

import qualified Data.Falsify.List  as List
import qualified Data.Falsify.Tree  as Tree
import qualified Test.Falsify.Range as Range

{-------------------------------------------------------------------------------
  Taking advantage of 'Selective'
-------------------------------------------------------------------------------}

-- | Generate a value with one of two generators
--
-- Shrinks towards the first generator;the two generators can shrink
-- independently from each other.
--
-- === Background
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

-- | Generate a value with one of many generators
--
-- Uniformly selects a generator and shrinks towards the first one.
oneof :: NonEmpty (Gen a) -> Gen a
oneof gens = frequency $ map (1,) $ NE.toList gens

{-------------------------------------------------------------------------------
  Auxiliary: marking elements
-------------------------------------------------------------------------------}

-- | Start with @Just x@ for some @x@, then shrink to @Nothing@
shrinkToNothing :: Gen a -> Gen (Maybe a)
shrinkToNothing g = firstThen Just (const Nothing) <*> g

-- | Mark an element, shrinking towards 'Drop'
--
-- This is similar to 'shrinkToNothing', except that 'Marked' still has a value
-- in the 'Drop' case: marks are merely hints, that we may or may not use.
mark :: Gen a -> Gen (Marked Gen a)
mark x = flip Marked x <$> firstThen Keep Drop

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

-- | Generate list of specified length
--
-- Shrinking behaviour:
--
-- * The length of the list will shrink as specified by the given range.
-- * We can drop random elements from the list, but prefer to drop them
--   from near the /end/ of the list.
--
-- Note on shrinking predictability: in the case that the specified 'Range' has
-- an origin which is neither the lower bound nor the upper bound (and only in
-- that case), 'list' can have confusing shrinking behaviour. For example,
-- suppose we have a range @(0, 10)@ with origin 5. Then we could start by
-- generating an intermediate list of length of 10 and then subsequently drop 5
-- elements from that, resulting in an optimal list length. However, we can now
-- shrink that length from 10 to 2 (which is closer to 5, after all), but now we
-- only have 2 elements to work with, and hence the generated list will now drop
-- from 5 elements to 2. This is not necessarily a problem, because that length
-- 2 can now subsequently shrink further towards closer to the origin (5), but
-- nonetheless it might result in confusing intermediate shrinking steps.
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
    n <- integral len

    -- Generate @n@ marks, indicating for each element if we want to keep that
    -- element or not, so that we can drop elements from the middle of the list.
    --
    -- Due to the left-biased nature of shrinking, this will shrink towards
    -- dropped elements (@False@ values) near the start, but we want them near
    -- the /end/, so we reverse the list.
    marks <- fmap (List.keepAtLeast (Range.origin len) . reverse) $
               replicateM (fromIntegral n) $ mark gen

    -- Finally, generate the elements we want to keep
    catMaybes <$> selectAllKept marks

-- | Choose random element
--
-- Shrinks towards earlier elements.
--
-- NOTE: Does not work on infinite lists (it computes the length of the list).
elem :: NonEmpty a -> Gen a
elem = fmap (\(_before, x, _after) -> x) . pick

-- | Generalization of 'elem' that additionally returns the parts of the list
-- before and after the element
pick :: NonEmpty a -> Gen ([a], a, [a])
pick = \xs ->
    aux [] (NE.toList xs) <$>
      integral (Range.between (0, length xs - 1))
  where
    aux :: [a] -> [a] -> Int -> ([a], a, [a])
    aux _    []     _ = error "pick: impossible"
    aux prev (x:xs) 0 = (reverse prev, x, xs)
    aux prev (x:xs) i = aux (x:prev) xs (i - 1)

-- | Choose random element from a list
--
-- This is different from 'elem': it avoids first computing the length of the
-- list, and is biased towards elements earlier in the list. The advantage is
-- that this works for infinite lists, too.
--
-- Also returns the elements from the list before and after the chosen element.
pickBiased :: NonEmpty a -> Gen ([a], a, [a])
pickBiased = \xs -> pickChunk [] (List.chunksOfNonEmpty chunkSize xs)
  where
    chunkSize :: Word
    chunkSize = 1_000

    -- We want to avoid computing the length of the list, but equally we don't
    -- want to skew /too/ heavily towards the start of the list. Therefore we
    -- chunk the list (this is lazy), then flip a coin for each chunk, and once
    -- we find a chunk, do an unbiased choice within that chunk.
    pickChunk :: [NonEmpty a] -> NonEmpty (NonEmpty a) -> Gen ([a], a, [a])
    pickChunk prev (chunk :| []) = do
        -- No choice left: we must generate use this chunk
        withChunk prev chunk []
    pickChunk prev (chunk :| next@(n:ns)) = do
        useChunk <- bool True
        if useChunk
          then withChunk prev chunk next
          else pickChunk (chunk:prev) (n :| ns)

    withChunk :: [NonEmpty a] -> NonEmpty a -> [NonEmpty a] -> Gen ([a], a, [a])
    withChunk prev chunk next = do
        (chunkBefore, chunkElem, chunkAfter) <- pick chunk
        return (
            concat $ reverse $ chunkBefore : map NE.toList prev
          , chunkElem
          , chunkAfter ++ concatMap NE.toList next
          )

{-------------------------------------------------------------------------------
  Tweak test data distribution
-------------------------------------------------------------------------------}

-- | Choose generator with the given frequency
--
-- For example,
--
-- > frequency [
-- >     (1, genA)
-- >   , (2, genB)
-- >   ]
--
-- will use @genA@ 1/3rd of the time, and @genB@ 2/3rds.
--
-- Shrinks towards generators earlier in the list; the generators themselves
-- are independent from each other (shrinking of @genB@ does not affect
-- shrinking of @genA@).
--
-- Precondition: there should at least one generator with non-zero frequency.
frequency :: forall a. [(Word, Gen a)] -> Gen a
frequency gens =
    case filter ((/= 0) . fst) indexedGens of
      []    -> error "frequency: no generators with non-zero frequency"
      gens' -> do
        let r :: Range Word
            r = Range.between (0, sum (map fst gens') - 1)
        (gen, genIx) <- (\i -> frequencyLookup i gens') <$> integral r
        perturb genIx gen
  where
    -- We need to be careful: we don't want to perturb the generator by the
    -- value generated by 'integral', because many different values could
    -- correspond to the /same/ generator. Instead, we assign each generator its
    -- own index, and use that instead.
    indexedGens :: [(Word, (Gen a, Word))]
    indexedGens = zipWith (\(f, g) i -> (f, (g, i))) gens [0..]

-- | Internal auxiliary to 'frequency'
frequencyLookup :: Word -> [(Word, x)] -> x
frequencyLookup = \i xs ->
    case go i xs of
      Just x  -> x
      Nothing ->
        error $ concat [
           "frequencyLookup: index "
         , show i
         , " out of range of "
         , show (map fst xs)
         ]
  where
    go :: Word -> [(Word, x)] -> Maybe x
    go _ []       = Nothing
    go i ((n, x):xs)
      | i < n     = Just x
      | otherwise = go (i - n) xs

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
    flip List.applyPermutation xs <$>
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
permutation n = do
    swaps <- mapM (mark . genSwap) [n - 1, n - 2 .. 1]
    catMaybes <$> selectAllKept swaps
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
    t <- Tree.keepAtLeast (Range.origin size) . Tree.propagate <$> go n
    Tree.genKept t
  where
    go :: Word -> Gen (Tree (Marked Gen a))
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
    -- We only want to pick a shrunk value that matches the predicate, but we
    -- potentially waste a /lot/ of work if we first evaluate the predicate for
    -- /all/ potential shrunk values and then choose. So, instead we choose
    -- first, evaluate the predicate, and if it fails, choose again.
    go :: p -> [Rose.Tree a] -> Gen (NonEmpty p)
    go p []     = pure (p :| [])
    go p (a:as) = do
        (before, a', after) <- pickBiased (a :| as)

        case checkPred a' of
          Nothing ->
            -- Not a valid shrink step. Pick a different one.
            go p (before ++ after)
          Just (p', as') ->
            -- Found a valid shrink step.
            --
            -- We only call @choose@ once we found a valid shrink step,
            -- otherwise we would skew very heavily towards shorter paths.
            choose
              (pure (p :| []))
              (NE.cons p <$> go p' as')

    checkPred :: Rose.Tree a -> Maybe (p, [Rose.Tree a])
    checkPred (Rose.Node a as) =
       case validShrink a of
         InvalidShrink _ -> Nothing
         ValidShrink   b -> Just (b, as)

-- | Variation on 'path' without a predicate.
pathAny :: ShrinkTree a -> Gen (NonEmpty a)
pathAny = fmap (either absurd id) . path ValidShrink

