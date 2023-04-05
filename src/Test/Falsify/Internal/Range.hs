-- | Internal 'Range' API
module Test.Falsify.Internal.Range (
    -- * Definition
    Range(..)
  , ProperFraction(ProperFraction)
  , Precision(..)
  ) where

import Data.Word
import GHC.Show
import GHC.Stack

{-------------------------------------------------------------------------------
  Proper frations
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

-- | See construction functions in the public API for documentation
data Range a =
    Constant a
  | FromProperFraction Precision (ProperFraction -> a)
  | Towards a [Range a]
  deriving stock (Functor)
