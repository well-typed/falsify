-- | Internal 'Range' API
module Test.Falsify.Internal.Range (
    -- * Definition
    Range(..)
  , ProperFraction(ProperFraction)
  , Precision(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Word
import GHC.Show
import GHC.Stack

{-------------------------------------------------------------------------------
  Proper fractions
-------------------------------------------------------------------------------}

-- | Value @x@ such that @0 <= x < 1@
newtype ProperFraction = UnsafeProperFraction { getProperFraction :: Double }
  deriving stock (Eq, Ord)
  deriving newtype (Num, Fractional)

-- | Show instance relies on the 'ProperFraction' pattern synonym
instance Show ProperFraction where
  showsPrec p (UnsafeProperFraction f) = showParen (p >= appPrec1) $
        showString "ProperFraction "
      . showsPrec appPrec1 f

mkProperFraction :: HasCallStack => Double -> ProperFraction
mkProperFraction f
  | 0 <= f && f < 1 = UnsafeProperFraction f
  | otherwise = error $ "mkProperFraction: not a proper fraction: " ++ show f

pattern ProperFraction :: Double -> ProperFraction
pattern ProperFraction f <- (getProperFraction -> f)
  where
    ProperFraction = mkProperFraction

{-# COMPLETE ProperFraction #-}

{-------------------------------------------------------------------------------
  Precision
-------------------------------------------------------------------------------}

-- | Precision (in bits)
newtype Precision = Precision Word8
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Enum)

{-------------------------------------------------------------------------------
  Range
-------------------------------------------------------------------------------}

-- | Range of values
data Range a where
  -- | Constant (point) range
  Constant :: a -> Range a

  -- | Construct values in the range from a 'ProperFraction'
  --
  -- This is the main constructor for 'Range'.
  FromProperFraction :: Precision -> (ProperFraction -> a) -> Range a

  -- | Evaluate each range and choose the \"smallest\"
  --
  -- Each value in the range is annotated with some distance metric; for
  -- example, this could be the distance to some predefined point (e.g. as in
  -- 'Test.Falsify.Range.towards')
  Smallest :: Ord b => NonEmpty (Range (a, b)) -> Range a

deriving stock instance Functor Range
