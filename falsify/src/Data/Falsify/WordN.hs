-- | N-bit words
--
-- Intended for qualified import.
--
-- > import Data.Falsify.WordN (WordN)
-- > import qualified Data.Falsify.WordN as WordN
module Data.Falsify.WordN (
    WordN -- opaque
  , Precision(..)
  , forgetPrecision
    -- * Construction
  , zero
  , truncateAt
  , unsafeFromWord64
    -- * Using
  , toProperFraction
  ) where

import Data.Bits
import Data.Word

import Data.Falsify.ProperFraction (ProperFraction(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Precision (in bits)
newtype Precision = Precision Word8
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Enum)

-- | @n@-bit word
data WordN = WordN Precision Word64
  deriving (Show, Eq, Ord)

-- | Forget the precision of the t'WordN'
forgetPrecision :: WordN -> Word64
forgetPrecision (WordN _ x) = x

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Zero can be represented at every precision
zero :: Precision -> WordN
zero p = WordN p 0

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

-- | Construct from 'Word64'
--
-- It is the caller's responsibility to ensure that the 'Word64' is in range.
unsafeFromWord64 :: Precision -> Word64 -> WordN
unsafeFromWord64 = WordN

{-------------------------------------------------------------------------------
  Using
-------------------------------------------------------------------------------}

-- | Compute fraction from @n@-bit word
toProperFraction :: WordN -> ProperFraction
toProperFraction (WordN (Precision p) x) =
    ProperFraction $ (fromIntegral x) / (2 ^ p)
