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
  , precisionRequiredToRepresent
  , WordN(..)
    -- ** Fractions
  , Fraction(..)
  , mkFraction
    -- * Generators
  , unsignedWordN
  , signedWordN
  , fraction
  , signedFraction
    -- * User-specified shrinking
  , shrinkTo
  , firstThen
  , shrinkWith
  ) where

import Data.Bits
import Data.Word

import qualified Data.Tree as Rose

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

precisionRequiredToRepresent :: forall a. FiniteBits a => a -> Precision
precisionRequiredToRepresent x = Precision $ fromIntegral $
    finiteBitSize (undefined :: a) - countLeadingZeros x

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

-- | Start with @x@, then shrink to any of the @xs@
--
-- Once shrunk, cannot shrink any further.
shrinkTo :: forall a. a -> [a] -> Gen a
shrinkTo x xs =
    aux <$> primWith shrinker
  where
    aux :: Sample -> a
    aux (NotShrunk _) = x
    aux (Shrunk    i) = xs !! fromIntegral i

    shrinker :: Sample -> [Word64]
    shrinker (Shrunk _)    = []
    shrinker (NotShrunk _)
               | null xs   = []
               | otherwise = [0 .. fromIntegral (length xs) - 1]

-- | Generator that always produces @x@ as initial value, and shrinks to @y@
firstThen :: forall a. a -> a -> Gen a
firstThen x y = x `shrinkTo` [y]

-- | Shrink with provided shrinker
--
-- This provides a way to do QuickCheck-style manual shrinking.
--
-- This is O(n^2) in the number of shrink steps: as this shrinks, the generator
-- is growing a path of indices which locates a particular value in the shrink
-- tree (resulting from unfolding the provided shrinking function). At each
-- step during the shrinking process the shrink tree is re-evaluated and the
-- next value in the tree is located; since this path throws linearly, the
-- overall cost is O(n^2).
--
-- The O(n^2) cost is only incurred on /locating/ the next element to be tested;
-- the property is not reevaluated at already-shrunk values.
shrinkWith :: forall a. (a -> [a]) -> Gen a -> Gen a
shrinkWith f gen = do
    x <- withoutShrinking gen
    go $ Rose.unfoldTree (\x' -> (x', f x')) x
  where
    go :: Rose.Tree a -> Gen a
    go (Rose.Node x []) = return x
    go (Rose.Node x xs) = do
        next <- Nothing `shrinkTo` map Just xs
        case next of
          Nothing -> return x
          Just x' -> go x'
