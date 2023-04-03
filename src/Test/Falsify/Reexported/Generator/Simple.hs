-- | Simple (i.e., non-compound) generators
module Test.Falsify.Reexported.Generator.Simple (
    bool
  , integral
  , int
  , enum
  ) where

import Data.Bits
import Data.List (minimumBy)
import Data.Ord
import Data.Word

import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Range
import Test.Falsify.SampleTree (Sample(..), sampleValue)
import qualified Test.Falsify.Range as Range

{-------------------------------------------------------------------------------
  Simple generators
-------------------------------------------------------------------------------}

-- | Generate random bool, shrink towards the given value
--
-- Chooses with equal probability between 'True' and 'False'.
bool :: Bool -> Gen Bool
bool target = aux . sampleValue <$> primWith shrinker
  where
    aux :: Word64 -> Bool
    aux x | msbSet x  = not target
          | otherwise = target

    msbSet :: forall a. FiniteBits a => a -> Bool
    msbSet x = testBit x (finiteBitSize (undefined :: a) - 1)

    shrinker :: Sample -> [Word64]
    shrinker (Shrunk 0) = []
    shrinker _          = [0]

{-------------------------------------------------------------------------------
  Integral ranges
-------------------------------------------------------------------------------}

fromFraction :: FiniteBits a => (Fraction -> a) -> a -> Gen a
fromFraction f delta = f <$> fraction (precisionRequiredToRepresent delta)

towards :: forall a. (Ord a, Num a) => a -> [Gen a] -> Gen a
towards origin gens =
    pick <$> sequence gens
  where
    pick :: [a] -> a
    pick [] = origin
    pick as = minimumBy (comparing distanceToOrigin) as

    distanceToOrigin :: a -> a
    distanceToOrigin x
      | x >= origin = x - origin
      | otherwise   = origin - x

-- | Generate value of integral type
integral :: (FiniteBits a, Integral a) => Range a -> Gen a
integral r =
    case r of
     Constant x     -> pure x
     FromFraction f -> fromFraction f (Range.delta r)
     Towards o rs   -> towards o (map integral rs)

-- | Type-specialization of 'integral'
int :: Range Int -> Gen Int
int = integral

-- | Generate value of enumerable type
--
-- For most types 'integral' is preferred; the 'Enum' class goes through 'Int',
-- and is therefore also limited by the range of 'Int'.
enum :: forall a. Enum a => Range a -> Gen a
enum r = toEnum <$> integral (fromEnum <$> r)
