-- | Numerical ranges
module Test.Falsify.Range (
    Range -- opaque
    -- * Primitive constructors
  , constant
  , fromFraction
  , towards
    -- * Constructing linear ranges
  , Fraction(..)
  , between
  , withOrigin
  , skewedBy
    -- * Queries
  , upperBound
  , lowerBound
  , delta
  , origin
  ) where

import Test.Falsify.Internal.Range

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
fromFraction :: (Fraction -> a) -> Range a
fromFraction = FromFraction

-- | Generate value in any of the specified ranges, then choose the one
-- that is closest to the specified origin
--
-- Precondition: the target must be within the bounds of all ranges.
towards :: a -> [Range a] -> Range a
towards = Towards

{-------------------------------------------------------------------------------
  Constructing ranges
-------------------------------------------------------------------------------}

-- | Uniform selection between the given bounds, shrinking towards first bound
between :: forall a. Integral a => (a, a) -> Range a
between = skewedBy 0

-- | Selection within the given bounds, shrinking towards the specified origin
withOrigin :: Integral a => (a, a) -> a -> Range a
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
          between (o, x)
        , between (o, y)
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
  but also has some reasonal properties. Suppose a skew of @s@ means that we
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
skewedBy :: Integral a => Double -> (a, a) -> Range a
skewedBy s (x, y)
  | x == y    = constant x
  | x < y     = fromFraction $ \(Fraction f) -> round $ x' + skew f * (y' - x')
  | otherwise = fromFraction $ \(Fraction f) -> round $ x' - skew f * (x' - y')
  where
    x', y' :: Double
    x' = fromIntegral x
    y' = fromIntegral y

    pos, neg :: Double -> Double
    pos f = 1 - ((1 -      f  ** (s + 1)) ** (1 / (    s + 1)))
    neg f =      (1 - (1 - f) ** (s + 1)) ** (1 / (abs s + 1))

    skew :: Double -> Double
    skew | s >= 0    = pos
         | otherwise = neg

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Maximum value in the range
upperBound :: Ord a => Range a -> a
upperBound (Constant x) =
    x
upperBound (FromFraction f) =
    -- This relies on the precondition to 'fromFraction'
    -- (monotonically increasing or decreasing)
    max (f minBound) (f maxBound)
upperBound (Towards o rs) =
    -- This relies on the precondition to 'towards'
    -- (origin within the bounds of all ranges).
    maximum (o : map upperBound rs)

-- | Minimum value in the range
lowerBound :: Ord a => Range a -> a
lowerBound (Constant x)      = x
lowerBound (FromFraction f) = min (f minBound) (f maxBound)
lowerBound (Towards o rs)   = minimum (o : map lowerBound rs)

-- | Distance between the upper bound and the lower bound
delta  :: (Ord a, Num a) => Range a -> a
delta r = upperBound r - lowerBound r

-- | Origin of the range (value we shrink towards)
origin ::  Range a -> a
origin (Constant x)     = x
origin (FromFraction f) = f minBound
origin (Towards o _)    = o

