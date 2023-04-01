-- | Numerical ranges
module Test.Falsify.Range (
    Range(..)
    -- * Constructing linear ranges
  , between
  , withOrigin
    -- * Queries
  , upperBound
  , origin
  ) where

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Range a =
    -- | Between selection between the two bounds, shrinking towards the first
    Between a a

    -- | Generate value in any of the specified ranges, then choose the one
    -- that is closest to the specified origin
  | Towards a [Range a]
  deriving stock (Show, Functor)

{-------------------------------------------------------------------------------
  Constructing ranges
-------------------------------------------------------------------------------}

-- | Selection within the given bounds, shrinking towards the specified origin
--
-- Implementation note: this splits the range into two halves, and then uses
-- 'Towards' to combine them. We are careful to split the range /only/ when
-- needed: if we don't (i.e., if the origin /equals/ one of the endpoints),
-- then that would result in a singleton range, and since that singleton range
-- (by definition) would be at the origin, we would only ever produce that
-- one value.
withOrigin :: Eq a => (a, a) -> a -> Range a
withOrigin (x, y) o
  | o == x    = between (x, y)
  | o == y    = between (y, x)
  | otherwise = Towards o [
                    Between o x
                  , Between o y
                  ]

-- | Uniform selection between the given bounds, shrinking towards first bound
between :: (a, a) -> Range a
between (x, y) = Between x y

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Maximum value in the range
upperBound :: Ord a => Range a -> a
upperBound (Between x y)  = max x y
upperBound (Towards o rs) = maximum (o : map upperBound rs)

-- | Origin of the range (value we shrink towards)
origin ::  Range a -> a
origin (Between x _) = x
origin (Towards o _) = o