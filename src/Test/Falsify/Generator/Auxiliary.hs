-- | Auxiliary generators
--
-- The generators in this module are primarily intended to be used as building
-- blocks in other generators, and provide less control over ranges, shrinking
-- etc. than the higher level generators do. Most users probably do not need to
-- use the generators in this module.
--
-- Intended for unqualified import.
module Test.Falsify.Generator.Auxiliary (
    -- * Supporting types
    -- ** Fractions
    Fraction(..)
  , mkFraction
    -- ** Word63
  , Word63(..)
    -- ** Signed values
  , Signed(..)
  , sign
  , magnitude
    -- * Generators
  , signedWord63
  , fraction
  , signedFraction
  ) where

import Data.Bits
import Data.Word

import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Search

{-------------------------------------------------------------------------------
  Supporting type: fractions
-------------------------------------------------------------------------------}

-- | Value in the range [0 .. 1]
newtype Fraction = Fraction Double
  deriving stock (Show, Eq, Ord)

mkFraction :: (Integral a, FiniteBits a) => a -> Fraction
mkFraction x = Fraction $ fromIntegral x / (2 ^ finiteBitSize x - 1)

{-------------------------------------------------------------------------------
  Supporting type: Word63
-------------------------------------------------------------------------------}

-- | 63-bit word
newtype Word63 = Word63 Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Enum, Integral, Bits)

instance FiniteBits Word63 where
  finiteBitSize _ = 63

instance Bounded Word63 where
  minBound = Word63 0
  maxBound = Word63 (maxBound `div` 2)

{-------------------------------------------------------------------------------
  Supporting type: signed values
-------------------------------------------------------------------------------}

-- | Signed value
--
-- Depending on @a@, there is redundancy in this representation: @Pos 0@ and
-- @Neg 0@ represent the same value, for instance, so that 'Signed Word63' is
-- nearly but not quite isomorphic to 'Int64'.
data Signed a = Pos a | Neg a
  deriving stock (Functor, Show, Eq, Ord)

sign :: Signed a -> Signed ()
sign = fmap (const ())

magnitude :: Signed a -> a
magnitude (Pos x) = x
magnitude (Neg x) = x

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | Generate 'Signed Word63', shrinking towards 0
--
-- Shrinking will decrease the /magnitude/ (distance to 0), but may randomly
-- fluctuate the /sign/: there is no bias towards negative or positive.
signedWord63 :: Gen (Signed Word63)
signedWord63 =
    aux <$> primWith binarySearchWithoutParityBias
  where
    -- As @x@ tends towards 0, the LSB of @x@ (i.e., whether @x@ is even or not)
    -- will fluctuate randomly. Thus, we can use this to determine whether we
    -- want a positive or negative number, and then can use the remaining bits
    -- (shifted appropriately) for the magnitude.
    --
    -- Note that this is quite different from simply reinterpreting the unsigned
    -- number as a signed number; in this case, it would be the /most/
    -- significant bit that determines whether the number is negative or
    -- positive, and would therefore introduce a heavy bias towards positive
    -- numbers (not to mention that the direction of shrinking would be reversed
    -- due to two's complement).
    aux :: Word64 -> Signed Word63
    aux x = (if even x then Pos else Neg) $ Word63 (x `div` 2)

-- | Generate fraction, shrinking towards 0
fraction :: Gen Fraction
fraction = mkFraction <$> prim

-- | Generate signed fraction, shrinking towards 0
--
-- There is no bias towards positive or negative fractions. See 'signedWord63'
-- for more detailed discussion.
signedFraction :: Gen (Signed Fraction)
signedFraction = fmap mkFraction <$> signedWord63
