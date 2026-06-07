-- | Numerical ranges
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Range (Range)
-- > import qualified Test.Falsify.Range as Range
module Test.Falsify.Range (
    Range -- opaque
    -- * Constructors
    -- ** Linear
  , uniform
  , between
  , enum
  , withOrigin
    -- ** Non-linear
  , skewedBy
    -- * Queries
  , origin
    -- * Primitive constructors
  , constant
  , fromProperFraction
  , towards
    -- * Evalation
  , eval
  ) where

import Data.Bits
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord
import Data.Word

import qualified Data.List.NonEmpty as NE

import Data.Falsify.ProperFraction (ProperFraction(..))
import Data.Falsify.WordN (WordN)
import Test.Falsify.Internal.Range

import qualified Data.Falsify.WordN as WordN

{-------------------------------------------------------------------------------
  Primitive ranges
-------------------------------------------------------------------------------}

-- | Range that is @x@ everywhere
constant :: a -> Range a
constant = Constant

-- | Construct @a@ given a fraction
--
-- Precondition: @f@ must be monotonically increasing or decreasing; i.e.
--
-- * for all @x <= y@, @f x <= f y@, /or/
-- * for all @x <= y@, @f y <= f x@
fromProperFraction :: WordN.Precision -> (ProperFraction -> a) -> Range a
fromProperFraction p f = FromWordN p $ f . WordN.toProperFraction

-- | Generate value in any of the specified ranges, then choose the one
-- that is closest to the specified origin
--
-- Precondition: the target must be within the bounds of all ranges.
towards :: forall a. (Ord a, Num a) => a -> [Range a] -> Range a
towards o []     = Constant o
towards o (r:rs) = Smallest $ fmap aux (r :| rs)
  where
    aux :: Range a -> Range (a, a)
    aux = fmap $ \x -> (x, distanceToOrigin x)

    distanceToOrigin :: a -> a
    distanceToOrigin x
      | x >= o    = x - o
      | otherwise = o - x

{-------------------------------------------------------------------------------
  Constructing ranges
-------------------------------------------------------------------------------}

-- | Uniform selection anywhere in the full range @[minBound .. maxBound]@
--
-- Shrinks towards zero.
--
-- If you don't need specific bounds, you should probably use 'uniform' instead
-- of 'between', especially for large bit sizes, because we can more easily
-- guarantee a true uniform selection here.
uniform :: forall a. (Integral a, FiniteBits a, Bounded a) => Range a
uniform = FromWordN precision $ \x ->
    if isUnsigned
      then toUnsigned (WordN.forgetPrecision x)
      else toSigned   (WordN.forgetPrecision x)
  where
    precision :: WordN.Precision
    precision = WordN.Precision $ fromIntegral $ finiteBitSize (undefined :: a)

    isUnsigned :: Bool
    isUnsigned = signum (-1 :: a) == 1

    toUnsigned :: Word64 -> a
    toUnsigned = fromIntegral

    -- For signed numbers we must be careful. Consider Int8, starting with an
    -- 8-bit precision Word64. That Word64 might shrink from 255 to 254; if we
    -- just cast this to Int8, the corresponding values would be -1 and -2,
    -- violating the requirement that we shrink towards zero. We therefore need
    -- to "reflect" the negative range.
    --
    -- NOTE: 'fromIntegral' just does a bit-wise cast, e.g
    --
    -- >    fromIntegral (200 :: Word64) :: Int8
    -- > == (-56)
    toSigned :: Word64 -> a
    toSigned x
      | x <= maxPos = fromIntegral x
      | otherwise   = (maxBound :: a) - fromIntegral x
      where
        -- maxBound must fit within 64-bits
        -- (assuming @a@ does not exceed 64 bits, of course)
        maxPos :: Word64
        maxPos = fromIntegral (maxBound :: a)

-- | Uniform selection between the given bounds, shrinking towards first bound
--
-- See also 'uniform'.
between :: forall a. (Integral a, FiniteBits a) => (a, a) -> Range a
between = skewedBy 0

-- | Variation on 'between' for types that are 'Enum' but not 'Integral'
--
-- This is useful for types such as 'Char'. However, since this relies on
-- 'Enum', it's limited by the precision of 'Int'.
enum :: Enum a => (a, a) -> Range a
enum (x, y) = toEnum <$> between (fromEnum x, fromEnum y)

-- | Selection within the given bounds, shrinking towards the specified origin
--
-- All else being equal, prefers values in the /second/ half of the range
-- (in the common case of say @withOrigin (-100, 100) 0@, this means we prefer
-- positive values).
withOrigin :: (Integral a, FiniteBits a) => (a, a) -> a -> Range a
withOrigin (x, y) o
  | not originInBounds
  = error "withOrigin: origin not within bounds"

  -- Since origin must be within bounds, we must have x == o == y here
  | x == y
  = Constant x

  | o == x
  = between (x, y)

  | o == y
  = between (y, x)

-- Split the range into two halves. We are careful to do this only when needed:
-- if we didn't (i.e., if the origin /equals/ one of the endpoints), that would
-- result in a singleton range, and since that singleton range (by definition)
-- would be at the origin, we would only ever produce that one value.
  | otherwise =
      towards o [
          between (o, y)
        , between (o, x)
        ]
  where
    originInBounds :: Bool
    originInBounds
      | x <= o && o <= y = True
      | y <= o && o <= x = True
      | otherwise        = False

{-------------------------------------------------------------------------------
  Skew

  To introduce skew, we want something that is reasonably simply to implement
  but also has some reasonable properties. Suppose a skew of @s@ means that we
  generate value from the lower 20% of the range 60% of the time. Then:

  - Symmetry around the antidiagonal: we will generate a value from the
    upper 60% of the range 20% of the time.

  - Symmetry around the diagonal: a skew of @-s@ will mean we generate a value
    from the /upper/ 20% of the range 60% of the time.

  To derive the formula we use, suppose we start with a circle with radius 1,
  centered at the origin:

  > x^2 + y^2 == 1
  >       y^2 == 1 - x^2
  >       y   == (1 - x^2) ^ (1/2)

  In the interval [0, 1] this gives us the upper right quadrant of the circle,
  but we want the lower right:

  > y == 1 - ((1 - x^2) ^ (1/2))

  We can now vary that power.

  > y == 1 - ((1 - x^3) ^ (1/3))
  > y == 1 - ((1 - x^4) ^ (1/4))
  > ..

  If the power is 1, we get no skew:

  > y == 1 - ((1 - x^1) ^ (1/1))
  >   == 1 - (1 - x)
  >   == x

  We want a skew of 0 to mean no skew, so in terms of s:

  > y == 1 - ((1 - x^(s+1)) ^ (1/(s+1)))

  For negative values of @s@, we flip this around the diagonal:

  > y == 1 - (1 - ((1 - (1-x)^(s+1)) ^ (1/(s+1))))
  >   ==           (1 - (1-x)^(s+1)) ^ (1/(s+1))

  giving us

  > (1 - (1 - x)^2)^(1/2)  for s == -1
  > (1 - (1 - x)^3)^(1/3)  for s == -2
  > etc.
-------------------------------------------------------------------------------}

-- | Introduce skew (non-uniform selection)
--
-- A skew of @s == 0@ means no skew: uniform selection.
--
-- A positive skew @(s > 0)@ introduces a bias towards smaller values (this is
-- the typical use case). As example, for a skew of @s == 1@:
--
-- * We will generate a value from the lower 20% of the range 60% of the time.
-- * We will generate a value from the upper 60% of the range 20% of the time.
--
-- A negative skew @(s < 0)@ introduces a bias towards larger values. For a
-- skew of @s == 1@:
--
-- * We will generate a value from the upper 20% of the range 60% of the time.
-- * We will generate a value from the lower 60% of the range 20% of the time.
--
-- The table below lists values for the percentage of the range used, given a
-- percentage of the time (a value of 0 means a single value from the range):
--
-- >    | time%
-- >  s | 50% | 90%
-- > --------------
-- >  0 |  50 |  90
-- >  1 |  13 |  56
-- >  2 |   4 |  35
-- >  3 |   1 |  23
-- >  4 |   0 |  16
-- >  5 |   0 |  11
-- >  6 |   0 |   8
-- >  7 |   0 |   6
-- >  8 |   0 |   5
-- >  9 |   0 |   4
-- > 10 |   0 |   3
--
-- Will shrink towards @x@, independent of skew.
--
-- NOTE: The implementation currently uses something similar to μ-law encoding.
-- As a consequence, the generator gets increased precision near the end of the
-- range we skew towards, and less precision near the other end. This means that
-- not all values in the range can be produced.
skewedBy :: forall a. (FiniteBits a, Integral a) => Double -> (a, a) -> Range a
skewedBy s (x, y)
  | x == y    = constant x
  | x < y     = let p = precisionRequiredToRepresent (y - x)
                in fromProperFraction p $ \(ProperFraction f) -> roundDown f
  | otherwise = let p = precisionRequiredToRepresent (x - y)
                in fromProperFraction p $ \(ProperFraction f) -> roundUp   f
  where
    x', y' :: Double
    x' = fromIntegral x
    y' = fromIntegral y

    -- We have to be careful here. Perhaps the more obvious way to express this
    -- calculation is
    --
    -- > round $ x' + skew f * (y' - x')
    --
    -- However, this leads to a bad distribution of test data. Suppose we are
    -- generating values in the range [0 .. 2]. Then that call to 'round'
    -- would result in something like this:
    --
    -- >  0..............1..............2
    -- > [       /\             /\      ]
    -- >  ^^^^^^^^  ^^^^^^^^^^^^  ^^^^^^
    -- >     0            1           2
    --
    -- To avoid this heavy bias, we instead do this:
    --
    -- >  0..............1..............2..............3
    -- > [              /|             /|              /]
    -- >  ^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^
    -- >        0                1              2
    --
    -- By insisting that the fraction is a /proper/ fraction (i.e., not equal to
    -- 1), we avoid generating @3@ (which would be outside the range).
    roundDown, roundUp :: Double -> a
    roundDown f = floor   $ x' + skew f * (y' - x' + 1)
    roundUp   f = ceiling $ x' - skew f * (x' - y' + 1)

    pos, neg :: Double -> Double
    pos f = 1 - ((1 -      f  ** (s + 1)) ** (1 / (    s + 1)))
    neg f =      (1 - (1 - f) ** (s + 1)) ** (1 / (abs s + 1))

    skew :: Double -> Double
    skew | s == 0    = id
         | s >= 0    = pos
         | otherwise = neg

{-------------------------------------------------------------------------------
  Precision
-------------------------------------------------------------------------------}

-- | Precision required to be able to choose within the given range
--
-- In order to avoid rounding errors, we set a lower bound on the precision.
-- This lower bound is verified in "TestSuite.Sanity.Range", which verifies that
-- for small ranges, the expected distribution is never off by more than 1%
-- from the actual distribution.
--
-- TODO: it would be nicer to move this to "Data.Falsify.WordN", but this
-- lower bound of 7 bits is quite hacky. Ideally we'd have a better story here.
precisionRequiredToRepresent :: forall a. FiniteBits a => a -> WordN.Precision
precisionRequiredToRepresent x = fromIntegral $
    7 `max` (finiteBitSize (undefined :: a) - countLeadingZeros x)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Origin of the range (value we shrink towards)
origin ::  Range a -> a
origin = runIdentity . eval (\p -> Identity $ WordN.zero p)

{-------------------------------------------------------------------------------
  Evaluation
-------------------------------------------------------------------------------}

-- | Evaluate a range, given an action to generate fractions
--
-- Most users will probably never need to call this function.
eval :: forall f a.
     Applicative f
  => (WordN.Precision -> f WordN) -> Range a -> f a
eval genWordN = go
  where
    go :: forall x. Range x -> f x
    go r =
        case r of
          Constant x    -> pure x
          FromWordN p f -> f <$> genWordN p
          Smallest rs   -> smallest <$> sequenceA (fmap go rs)

    smallest :: Ord b => NonEmpty (x, b) -> x
    smallest = fst . NE.head . NE.sortBy (comparing snd)
