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
between (x, y)
  | x == y    = constant x
  | x <  y    = fromFraction $ \(Fraction f) -> round $ x' + f * (y' - x')
  | otherwise = fromFraction $ \(Fraction f) -> round $ x' - f * (x' - y')
  where
    x', y' :: Double
    x' = fromIntegral x
    y' = fromIntegral y

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

