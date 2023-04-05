-- | Fixed precision generators
module Test.Falsify.Reexported.Generator.Precision (
    -- * @n@-bit words
    WordN(..)
  , wordN
    -- ** Fractions
  , properFraction
  ) where

import Prelude hiding (properFraction)

import Data.Bits
import Data.Word
import GHC.Stack

import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Range
import Test.Falsify.Internal.SampleTree (sampleValue)
import Test.Falsify.Internal.Search

{-------------------------------------------------------------------------------
  @n@-bit word
-------------------------------------------------------------------------------}

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

-- | Uniform selection of @n@-bit word of given precision, shrinking towards 0
wordN :: Precision -> Gen WordN
wordN p =
    fmap (truncateAt p . sampleValue) . primWith $
        binarySearch
      . forgetPrecision
      . truncateAt p
      . sampleValue

{-------------------------------------------------------------------------------
  Fractions
-------------------------------------------------------------------------------}

-- | Compute fraction from @n@-bit word
mkFraction :: WordN -> ProperFraction
mkFraction (WordN (Precision p) x) =
    ProperFraction $ (fromIntegral x) / (2 ^ p)

-- | Uniform selection of fraction, shrinking towards 0
--
-- Precondition: precision must be at least 1 bit (a zero-bit number is constant
-- 0; it is meaningless to have a fraction in a point range).
properFraction :: HasCallStack => Precision -> Gen ProperFraction
properFraction (Precision 0) = error "fraction: 0 precision"
properFraction p             = mkFraction <$> wordN p
