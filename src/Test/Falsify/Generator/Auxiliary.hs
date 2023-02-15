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
  , precisionOf
  , WordN(..)
    -- ** Fractions
  , Fraction(..)
  , mkFraction
    -- * Generators
  , unsignedWordN
  , signedWordN
  , fraction
  , signedFraction
    -- * Specialized shrinking behaviour
  , firstThen
  ) where

import Data.Bits
import Data.Word

import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Search
import Test.Falsify.SampleTree (Sample(..), sampleValue)

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

precisionOf :: forall proxy a. FiniteBits a => proxy a -> Precision
precisionOf _ = Precision (fromIntegral $ finiteBitSize (undefined :: a))

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

-- | Value in the range [0 .. 1]
newtype Fraction = Fraction Double
  deriving stock (Show, Eq, Ord)

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

-- | Uniform selection of signed @n-bit@ word, shrinking towards 0
--
-- Shrinking will decrease the /magnitude/ (distance to 0), but may randomly
-- fluctuate the /sign/: there is no bias towards negative or positive.
--
-- Maximum precision available is @n == 63@.
signedWordN :: Precision -> Gen (Signed WordN)
signedWordN = \p ->
    -- We will use the LSB to determine the sign of the value, so we must ask
    -- for one more bit of precision.
    let p' = succ p
    in fmap (aux . truncateAt p' . sampleValue) . primWith $
            binarySearchNoParityBias
          . forgetPrecision
          . truncateAt p'
          . sampleValue
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

-- | Uniform selection of fraction, shrinking towards 0
fraction :: Precision -> Gen Fraction
fraction p = mkFraction <$> unsignedWordN p

-- | Uniform selection of signed fraction, shrinking towards 0
--
-- There is no bias towards positive or negative fractions. See 'signedWordN'
-- for more detailed discussion.
signedFraction :: Precision -> Gen (Signed Fraction)
signedFraction p = fmap mkFraction <$> signedWordN p

{-------------------------------------------------------------------------------
  Specialized shrinking behaviour
-------------------------------------------------------------------------------}

-- | Generator that always produces @x@ as initial value, and shrinks to @y@
firstThen :: forall a. a -> a -> Gen a
firstThen x y =
    aux <$> primWith shrinker
  where
    aux :: Sample -> a
    aux (NotShrunk _) = x
    aux (Shrunk    _) = y

    shrinker :: Sample -> [Word64]
    shrinker (NotShrunk _) = [0] -- we could pick any value here really
    shrinker (Shrunk    _) = []

