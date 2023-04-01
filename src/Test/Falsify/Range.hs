-- | Numerical ranges
module Test.Falsify.Range (
    Range(..)
  , linear
  , between
  ) where

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Range a =
    Linear {
        lo     :: a
      , hi     :: a
      , origin :: a
      }
  deriving (Functor)

{-------------------------------------------------------------------------------
  Constructing linear ranges
-------------------------------------------------------------------------------}

-- | Linear range with specified origin and lo and hi bounds
linear :: Ord a => (a, a) -> a -> Range a
linear (lo, hi) origin
  | lo <= hi  = Linear{origin, lo, hi}
  | otherwise = error "linear: lo > hi"

-- | Linear range between given bounds, with origin at the first bound
between :: Ord a => (a, a) -> Range a
between (x, y)
  | x <= y    = linear (x, y) x
  | otherwise = linear (y, x) x
