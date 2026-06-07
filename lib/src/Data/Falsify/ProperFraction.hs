-- | Proper fractions
--
-- Intended for qualified import.
--
-- > import Data.Falsify.ProperFraction (ProperFraction(..))
module Data.Falsify.ProperFraction (
    ProperFraction(ProperFraction)
  ) where

import Prelude hiding (properFraction)

import GHC.Show
import GHC.Stack

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Value @x@ such that @0 <= x < 1@
newtype ProperFraction = UnsafeProperFraction { getProperFraction :: Double }
  deriving stock (Eq, Ord)
  deriving newtype (Num, Fractional)

-- | Show instance relies on the v'ProperFraction' pattern synonym
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
