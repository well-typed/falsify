-- | Simple (i.e., non-compound) generators
module Test.Falsify.Reexported.Generator.Simple (
    bool
  , integral
  , enum
    -- * Auxiliary
  , integerWithPrecision
  , integerFromFraction
  ) where

import Data.Word
import Data.Bits

import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Range (Range(..), origin)
import Test.Falsify.SampleTree (Sample(..), sampleValue)

import qualified Test.Falsify.Range as Range

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

integerWithPrecision :: Precision -> Range Integer -> Gen Integer
integerWithPrecision p r = integerFromFraction r <$> signedFraction p

integerFromFraction :: Range Integer -> Signed Fraction -> Integer
integerFromFraction r = \f -> if
  | origin r == lo r
  -> round $ lo' + getFraction (forgetSign f) * (hi' - lo')

  | origin r == hi r
  -> round $ hi' - getFraction (forgetSign f) * (hi' - lo')

  -- The shrinking behaviour of the general case is perhaps a little
  -- counter-intuitive. The property is that we will get closer to the origin
  -- /as a fraction of each half of the range/. However, if the sizes of the
  -- two ranges are wildly different, this might yield some strange results.
  -- For example, if we have a range such as
  --
  -- > 0     10                                 90
  -- > [     *                                   ]
  --
  -- then we could can from 50% of the left range (== 5) to any percentage
  -- less than 50% of the right range (== 50); for example, we might "shrink"
  -- from 5 to 45. This might seem a little strange, but it's unclear what a
  -- better definition is without introducing a bias between the two halves.
  | otherwise
  -> fromOrigin f
  where
    lo', hi', origin' :: Double
    lo'     = fromInteger $ lo     r
    hi'     = fromInteger $ hi     r
    origin' = fromInteger $ origin r

    fromOrigin :: Signed Fraction -> Integer
    fromOrigin (Neg (Fraction f)) = round $ origin' - f * (origin' - lo')
    fromOrigin (Pos (Fraction f)) = round $ origin' + f * (hi' - origin')

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

-- | Uniform selection of random value in the specified range
integral :: forall a. (Integral a, FiniteBits a) => Range a -> Gen a
integral r = fromIntegral <$>
    integerWithPrecision
      (precisionRequiredToRepresent $ hi r)
      (Range.fromIntegral r)

-- | Uniform selection of random value in the specified range
--
-- For most types 'integral' is preferred; the 'Enum' class goes through 'Int',
-- and is therefore also limited by the range of 'Int'.
enum :: forall a. Enum a => Range a -> Gen a
enum r = toEnum . fromIntegral <$>
    integerWithPrecision
      (precisionRequiredToRepresent $ fromEnum $ hi r)
      (Range.fromEnum r)
