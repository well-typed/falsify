-- | Simple (i.e., non-compound) generators
module Test.Falsify.Reexported.Generator.Simple (
    bool
  , inRange
  , integral
  , enum
  , int
  ) where

import Prelude hiding (properFraction)

import Data.Bits
import Data.Word

import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Range
import Test.Falsify.Internal.SampleTree (Sample(..), sampleValue)
import Test.Falsify.Reexported.Generator.Precision

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

-- | Generate value in the specified range
inRange :: Range a -> Gen a
inRange r = Range.eval properFraction r

-- | Deprecated alias for 'inRange'
integral :: Range a -> Gen a
{-# DEPRECATED integral "Use inRange instead" #-}
integral = inRange

-- | Deprecated alias for 'inRange'
enum :: Range a -> Gen a
{-# DEPRECATED enum "Use inRange instead" #-}
enum = inRange

-- | Type-specialization of 'inRange'
int :: Range Int -> Gen Int
int = inRange

