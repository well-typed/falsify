-- | Fixed precision generators
module Test.Falsify.Reexported.Generator.Precision (
    -- * @n@-bit words
    Precision(..)
  , WordN(..)
    -- * Construction and generation
  , truncateAt
  , wordN
  ) where

import Data.Bits
import Data.Word

import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.SampleTree (sampleValue)
import Test.Falsify.Internal.Search

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

forgetPrecision :: WordN -> Word64
forgetPrecision (WordN _ x) = x

{-------------------------------------------------------------------------------
  Construction and generation
-------------------------------------------------------------------------------}

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

-- | Uniform selection of @n@-bit word of given precision, shrinking towards 0
wordN :: Precision -> Gen WordN
wordN p =
    fmap (truncateAt p . sampleValue) . primWith $
        binarySearch
      . forgetPrecision
      . truncateAt p
      . sampleValue
