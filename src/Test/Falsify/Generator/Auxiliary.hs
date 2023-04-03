{-# LANGUAGE TypeOperators #-}
-- | Auxiliary generators
--
-- The generators in this module are primarily intended to be used as building
-- blocks in other generators, and provide less control over ranges, shrinking
-- etc. than the higher level generators do. Most users probably do not need to
-- use the generators in this module.
--
-- Intended for unqualified import.
module Test.Falsify.Generator.Auxiliary (
    -- * Auxiliary types
    -- ** Signed values
    Signed(..)
  , forgetSign
    -- ** @n@-bit words
  , Precision(..)
  , precisionRequiredToRepresent
  , WordN(..)
    -- ** Fractions
  , Fraction(..)
  , mkFraction
    -- * Generators
    -- ** @n@-bit words
  , unsignedWordN
    -- ** Fractions
  , fraction
    -- * User-specified shrinking
  , shrinkToOneOf
  , firstThen
  , shrinkWith
    -- * Support for shrink trees
  , fromShrinkTree
  , toShrinkTree
  ) where

import Data.Bits
import Data.Word
import GHC.Stack

import qualified Data.Tree as Rose

import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Generator.Truncated (Truncated)
import Test.Falsify.Internal.Search
import Test.Falsify.Internal.Range
import Test.Falsify.SampleTree (Sample(..), sampleValue, SampleTree)

{-------------------------------------------------------------------------------
  Auxiliary type: signed values
-------------------------------------------------------------------------------}

-- | Signed value
--
-- Depending on @a@, there is redundancy in this representation: @Pos 0@ and
-- @Neg 0@ represent the same value, for instance, so that 'Signed Word63' is
-- nearly but not quite isomorphic to 'Int64'.
data Signed a = Pos a | Neg a
  deriving stock (Functor, Show, Eq)

forgetSign :: Signed a -> a
forgetSign (Pos x) = x
forgetSign (Neg x) = x

{-------------------------------------------------------------------------------
  Auxiliary type: @n@-bit word
-------------------------------------------------------------------------------}

-- | Precision (in bits)
newtype Precision = Precision Word8
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Enum)

precisionRequiredToRepresent :: forall a. FiniteBits a => a -> Precision
precisionRequiredToRepresent x = Precision $ fromIntegral $
    finiteBitSize (undefined :: a) - countLeadingZeros x

-- | @n@-bit word
data WordN = WordN Precision Word64
  deriving (Show, Eq, Ord)

forgetPrecision :: WordN -> Word64
forgetPrecision (WordN _ x) = x

-- | Make @n@-bit word (@n <= 64@)
--
-- Bits outside the requested precision will be zeroed.
--
-- We use this to generate random @n@-bit words from random 64-bit words.
-- It is important that we /truncate/ rather than /cap/ the value: capping the
-- value (limiting it to a certain maximum) would result in a strong bias
-- towards that maximum value.
--
-- Of course, /shrinking/ of a Word64 bit does not translate automatically to
-- shrinking of the lower @n@ bits of that word (a decrease in the larger
-- 'Word64' may very well be an /increase/ in the lower @n@ bits), so this must
-- be taken into account.
truncateAt :: Precision -> Word64 -> WordN
truncateAt desiredPrecision x =
    WordN actualPrecision (x .&. mask actualPrecision)
  where
    maximumPrecision, actualPrecision :: Precision
    maximumPrecision = Precision 64
    actualPrecision  = min desiredPrecision maximumPrecision

    -- Maximum possible value
    --
    -- If @n == 64@ then @2 ^ n@ will overflow, but it will overflow to @0@, and
    -- @(-1) :: Word64 == maxBound@; so no need to treat this case separately.
    mask :: Precision -> Word64
    mask (Precision n) = 2 ^ n - 1

{-------------------------------------------------------------------------------
  Auxiliary type: fractions
-------------------------------------------------------------------------------}

-- | Compute fraction from @n@-bit word
mkFraction :: WordN -> Fraction
mkFraction (WordN (Precision p) x) = Fraction $ (fromIntegral x) / (2 ^ p - 1)

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | Uniform selection of @n@-bit word of given precision, shrinking towards 0
unsignedWordN :: Precision -> Gen WordN
unsignedWordN p =
    fmap (truncateAt p . sampleValue) . primWith $
        binarySearch
      . forgetPrecision
      . truncateAt p
      . sampleValue

-- | Uniform selection of fraction, shrinking towards 0
--
-- Precondition: precision must be at least 1 bit (a zero-bit number is constant
-- 0; it is meaningless to have a fraction in a point range).
fraction :: HasCallStack => Precision -> Gen Fraction
fraction (Precision 0) = error "fraction: 0 precision"
fraction p             = mkFraction <$> unsignedWordN p

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
    fromShrinkTree $ Rose.unfoldTree (\x' -> (x', f x')) x

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
fromShrinkTree :: forall a. Rose.Tree a -> Gen a
fromShrinkTree = go
  where
    go :: Rose.Tree a -> Gen a
    go (Rose.Node x xs) = do
        next <- Nothing `shrinkToOneOf` map Just xs
        case next of
          Nothing -> return x
          Just x' -> go x'

-- | Expose the full shrink tree of a generator
--
-- This generator does not shrink.
toShrinkTree :: forall a. Gen a -> Gen (Rose.Tree a)
toShrinkTree gen =
    Rose.unfoldTree aux . runGen gen <$> captureLocalTree
  where
    aux :: (a, Truncated, [SampleTree]) -> (a,[(a, Truncated, [SampleTree])])
    aux (x, _, shrunk) = (x, map (runGen gen) shrunk)
