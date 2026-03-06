module Test.Falsify.Internal.ProperFraction (
    ProperFraction(ProperFraction)
    -- * Construction and generation
  , mkFraction
  , properFraction
  ) where

import Prelude hiding (properFraction)

import GHC.Show
import GHC.Stack

import Test.Falsify.Reexported.Generator.Precision
import Test.Falsify.Internal.Generator

{-------------------------------------------------------------------------------
  Definition
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
  Construction
-------------------------------------------------------------------------------}

-- | Compute fraction from @n@-bit word
mkFraction :: WordN -> ProperFraction
mkFraction (WordN (Precision p) x) =
    ProperFraction $ (fromIntegral x) / (2 ^ p)

-- | Uniform selection of fraction, shrinking towards 0
--
-- Precondition: precision must be at least 1 bit (a zero-bit number is constant
-- 0; it is meaningless to have a fraction in a point range).
properFraction :: HasCallStack => Precision -> Gen ProperFraction
properFraction (Precision 0) = error "fraction: 0 precision"
properFraction p             = mkFraction <$> wordN p
