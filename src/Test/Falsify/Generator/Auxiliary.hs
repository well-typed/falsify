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
    -- ** @n@-bit words
  , Precision(..)
  , WordN(..)
    -- ** Fractions
  , Fraction(..)
  , mkFraction
    -- * Generators
  , unsignedWordN
  , signedWordN
  , fraction
  , signedFraction
  ) where

import Data.Word

import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Search

{-------------------------------------------------------------------------------
  Auxiliary type: signed values
-------------------------------------------------------------------------------}

-- | Signed value
--
-- Depending on @a@, there is redundancy in this representation: @Pos 0@ and
-- @Neg 0@ represent the same value, for instance, so that 'Signed Word63' is
-- nearly but not quite isomorphic to 'Int64'.
data Signed a = Pos a | Neg a
  deriving stock (Functor, Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Auxiliary type: @n@-bit word
-------------------------------------------------------------------------------}

-- | Precision (in bits)
newtype Precision = Precision Word8
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Enum)

-- | @n@-bit word
data WordN = WordN Precision Word64
  deriving (Show, Eq, Ord)

forgetPrecision :: WordN -> Word64
forgetPrecision (WordN _ x) = x

-- | Make @n@-bit word (@n <= 64@)
--
-- If the 'Word64' exceeds the specified range, it will be capped at the max.
--
-- > capAt 4 10 == WordN 4 10
-- > capAt 4 16 == WordN 4 15
capAt :: Precision -> Word64 -> WordN
capAt desiredPrecision x =
    WordN actualPrecision (min x $ maxValue actualPrecision)
  where
    maximumPrecision, actualPrecision :: Precision
    maximumPrecision = Precision 64
    actualPrecision  = min desiredPrecision maximumPrecision

    -- Maximum possible value
    --
    -- If @n == 64@ then @2 ^ n@ will overflow, but it will overflow to @0@, and
    -- @(-1) :: Word64 == maxBound@; so no need to treat this case separately.
    maxValue :: Precision -> Word64
    maxValue (Precision n) = 2 ^ n - 1

{-------------------------------------------------------------------------------
  Auxiliary type: fractions
-------------------------------------------------------------------------------}

-- | Value in the range [0 .. 1]
newtype Fraction = Fraction Double
  deriving stock (Show, Eq, Ord)

-- | Compute fraction from @n@-bit word
mkFraction :: WordN -> Fraction
mkFraction (WordN (Precision p) x) = Fraction $ (fromIntegral x) / (2 ^ p - 1)

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | Generate @n@-bit word of specified precision, shrinking towards 0
unsignedWordN :: Precision -> Gen WordN
unsignedWordN p =
    capAt p <$>
      primWith (binarySearch . forgetPrecision . capAt p)

-- | Generated signed @n-bit@ word, shrinking towards 0
--
-- Shrinking will decrease the /magnitude/ (distance to 0), but may randomly
-- fluctuate the /sign/: there is no bias towards negative or positive.
--
-- Maximum precision available is @n == 63@.
signedWordN :: Precision -> Gen (Signed WordN)
signedWordN = \p ->
    -- We will use the LSB to determine the sign of the value, so we must ask
    -- for one more bit of precision.
    aux . capAt (succ p) <$>
      primWith (binarySearchNoParityBias . forgetPrecision . capAt (succ p))
  where
    -- As @x@ tends towards 0, the LSB of @x@ (i.e., whether @x@ is even or not)
    -- will fluctuate randomly. Thus, we can use this to determine whether we
    -- want a positive or negative number, and then can use the remaining bits
    -- (shifted appropriately) for the magnitude.
    --
    -- This is quite different from simply reinterpreting the unsigned number as
    -- a signed number; in this case, it would be the /most/ significant bit
    -- that determines whether the number is negative or positive, and would
    -- therefore introduce a heavy bias towards positive numbers (and the
    -- direction of shrinking would be reversed due to two's complement).
    aux :: WordN -> Signed WordN
    aux (WordN p x) =
        (if even x then Pos else Neg) $
          WordN (pred p) (x `div` 2)

-- | Generate fraction, shrinking towards 0
fraction :: Gen Fraction
fraction = mkFraction <$> unsignedWordN 64

-- | Generate signed fraction, shrinking towards 0
--
-- There is no bias towards positive or negative fractions. See 'signedWord63'
-- for more detailed discussion.
signedFraction :: Gen (Signed Fraction)
signedFraction = fmap mkFraction <$> signedWordN 63
