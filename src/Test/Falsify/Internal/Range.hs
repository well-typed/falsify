module Test.Falsify.Internal.Range (
    Range(..)
  , Fraction(..)
  ) where

{-------------------------------------------------------------------------------
  Definition

  Range is defined here in an internal module, because the public module does
  not expose the constructors.
-------------------------------------------------------------------------------}

-- | Value in the range [0 .. 1]
newtype Fraction = Fraction { getFraction :: Double }
  deriving stock (Show, Eq, Ord)

instance Bounded Fraction where
  minBound = Fraction 0
  maxBound = Fraction 1

-- | See construction functions in the public API for documentation
data Range a =
    Constant a
  | FromFraction (Fraction -> a)
  | Towards a [Range a]
  deriving stock (Functor)

