-- | Simple (i.e., non-compound) generators
module Test.Falsify.Generator.Simple (
    bool
  , word8
    -- * Fractions
  , fraction
  , signedFraction
  ) where

import Data.Word

import Test.Falsify.Generator (GenT)
import Test.Falsify.Nudge
import Test.Falsify.Range (Range(..))
import Test.Falsify.Util

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range

{-------------------------------------------------------------------------------
  Generate fractions

  These are primarily useful as building blocks in other generators.
-------------------------------------------------------------------------------}

fraction :: Monad m => Word -> GenT m Fraction
fraction precision =
    mkFraction (precision * 2) . fromLittleEndian <$>
      Gen.bytes precision

signedFraction :: Monad m => Word -> GenT m (Signed Fraction)
signedFraction precision =
    fmap (mkFraction (precision * 2 - 1)) . signed . fromLittleEndian <$>
      Gen.bytes precision

{-------------------------------------------------------------------------------
  Simple generators
-------------------------------------------------------------------------------}

bool :: Monad m => Range Bool NoOffset -> GenT m Bool
bool Range{lo, hi, inverted} =
    aux <$> fraction precision
  where
    aux :: Double -> Bool
    aux f =
        if not inverted
          then if f < 0.5 then lo else hi
          else if f < 0.5 then hi else lo

    -- We really only need one bit, but we don't have bit-level granularity
    precision :: Word
    precision = 1

-- | Generate 'Word8'
word8 :: Monad m => Range Word8 Word -> GenT m Word8
word8 r =
    aux <$> signedFraction precision
  where
    -- We make the somewhat arbitrary decision to regard negative fractions
    -- as corresponding to the lower half of the interval, and positive
    -- fractions as corresponding to the upper half.
    aux :: Signed Double -> Word8
    aux (Negative f) = round $ origin' - f * (origin' - lo')
    aux (Positive f) = round $ origin' + f * (hi' - origin')

    lo', hi', origin' :: Double
    lo'     = fromIntegral $ Range.lo     r
    hi'     = fromIntegral $ Range.hi     r
    origin' = fromIntegral $ Range.origin r

    -- We ask for /two/ bytes rather than one, since 'signed' will distribute
    -- the precision evenly over the two halves of the interval @[lo .. origin)@
    -- and @[origin, hi]@, but these two halves might not be of equal size.
    precision :: Word
    precision = 2

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Value in the range [0 .. 1]
type Fraction = Double

-- | Turn (up to) @n@ bits of entropy into mkFraction [0..1]
mkFraction :: Integral a => Word -> a -> Fraction
mkFraction n e =
    point / range
  where
    range, point :: Double
    range = (2 ^ n) - 1
    point = fromIntegral e

data Signed a = Positive a | Negative a
  deriving (Functor)

-- | Produce signed random number from unsigned random number
--
-- Postcondition: if @x@ tends towards 0, then @signed x@ will tend towards 0,
-- with no particular bias for negative or positive.
--
-- Discussion: as @x@ tends towards 0, the LSB of @x@ (i.e., whether @x@ is
-- even or not) will fluctuate randomly. Thus, we can use this to determine
-- whether we want a positive or negative number, and then can use the remaining
-- bits (shifted appropriately) for the magnitude.
--
-- Note that this is quite different from simply reinterpreting the unsigned
-- number as a signed number; in this case, it would be the /most/ significant
-- bit that determines whether the number is negative or positive, and would
-- therefore introduce a heavy bias towards positive numbers (not to mention
-- that the direction of shrinking would be reserved due to two's complement).
signed :: Integral a => a -> Signed a
signed x = (if even x then Positive else Negative) $ (x `div` 2)
