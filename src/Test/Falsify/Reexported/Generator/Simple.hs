-- | Simple (i.e., non-compound) generators
module Test.Falsify.Reexported.Generator.Simple (
    bool
  , integral
  ) where

import Data.Word
import Data.Bits

import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Range (Range(..), origin)

{-------------------------------------------------------------------------------
  Simple generators
-------------------------------------------------------------------------------}

-- | Generate random bool, shrink towards the given value
--
-- Chooses with equal probability between 'True' and 'False'.
bool :: Bool -> Gen Bool
bool target = aux <$> prim
  where
    aux :: Word64 -> Bool
    aux x | msbSet x  = not target
          | otherwise = target

    msbSet :: forall a. FiniteBits a => a -> Bool
    msbSet x = testBit x (finiteBitSize (undefined :: a) - 1)

-- | Uniform selection of random value in the specified range
integral :: forall a. (Integral a, FiniteBits a) => Range a -> Gen a
integral r = aux <$> signedFraction (precisionOf r)
  where
    lo', hi', origin' :: Double
    lo'     = fromIntegral $ lo     r
    hi'     = fromIntegral $ hi     r
    origin' = fromIntegral $ origin r

    aux :: Signed Fraction -> a
    aux (Neg (Fraction f)) = round $ origin' - f * (origin' - lo')
    aux (Pos (Fraction f)) = round $ origin' + f * (hi' - origin')
