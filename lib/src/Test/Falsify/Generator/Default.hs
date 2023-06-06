{-# LANGUAGE UndecidableInstances #-}

module Test.Falsify.Generator.Default
  ( GenDefault (..)
  , ViaTag (..)
  , ViaIntegral (..)
  , ViaEnum (..)
  , ViaList (..)
  , ViaString (..)
  , ViaGeneric (..)
  ) where

import Control.Applicative (liftA2)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), (:+:) (..), (:*:) (..))
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import Data.Bits (FiniteBits)
import GHC.Exts (IsList (..), IsString (..))
import GHC.TypeLits (KnownNat, natVal, Nat)

class GenDefault tag a where
  -- | Default generator for @a@
  --
  -- The type-level @tag@ allows types @a@ to have multiple defaults.
  genDefault :: Proxy tag -> Gen a

-- | DerivingVia wrapper for types with default instances under other tags
newtype ViaTag tag' a = ViaTag {unViaTag :: a}

instance GenDefault tag' a => GenDefault tag (ViaTag tag' a) where
  genDefault _ = fmap ViaTag (genDefault @tag' Proxy)

-- | DerivingVia wrapper for Integral types
newtype ViaIntegral a = ViaIntegral {unViaIntegral :: a}

instance (Integral a, FiniteBits a, Bounded a) => GenDefault tag (ViaIntegral a) where
  genDefault _ = fmap ViaIntegral (Gen.inRange (Range.between (minBound, maxBound)))

-- | DerivingVia wrapper for Enum types
newtype ViaEnum a = ViaEnum {unViaEnum :: a}

instance (Enum a, Bounded a) => GenDefault tag (ViaEnum a) where
  genDefault _ = fmap ViaEnum (Gen.inRange (Range.enum (minBound, maxBound)))

-- | DerivingVia wrapper for FromList types
newtype ViaList l (mn :: Nat) (mx :: Nat) = ViaList {unViaList :: l}

instance (IsList l, GenDefault tag (Item l), KnownNat mn, KnownNat mx) => GenDefault tag (ViaList l mn mx) where
  genDefault p =
    let bn = fromInteger (natVal (Proxy @mn))
        bx = fromInteger (natVal (Proxy @mx))
    in fmap (ViaList . fromList) (Gen.list (Range.between (bn, bx)) (genDefault p))

-- | DerivingVia wrapper for FromString types
newtype ViaString s (mn :: Nat) (mx :: Nat) = ViaString {unViaString :: s}

instance (IsString s, GenDefault tag Char, KnownNat mn, KnownNat mx) => GenDefault tag (ViaString s mn mx) where
  genDefault p =
    let bn = fromInteger (natVal (Proxy @mn))
        bx = fromInteger (natVal (Proxy @mx))
    in fmap (ViaString . fromString) (Gen.list (Range.between (bn, bx)) (genDefault p))

class GGenDefault tag f where
  ggenDefault :: Proxy tag -> Gen (f a)

instance GGenDefault tag U1 where
  ggenDefault _ = pure U1

instance GGenDefault tag a => GGenDefault tag (M1 i c a) where
  ggenDefault = fmap M1 . ggenDefault

instance (GGenDefault tag a, GGenDefault tag b) => GGenDefault tag (a :*: b) where
  ggenDefault p = liftA2 (:*:) (ggenDefault p) (ggenDefault p)

instance (GGenDefault tag a, GGenDefault tag b) => GGenDefault tag (a :+: b) where
  ggenDefault p = Gen.choose (fmap L1 (ggenDefault p)) (fmap R1 (ggenDefault p))

instance GenDefault tag a => GGenDefault tag (K1 i a) where
  ggenDefault = fmap K1 . genDefault

-- | DerivingVia wrapper for Generic types
newtype ViaGeneric tag a = ViaGeneric {unViaGeneric :: a}

instance (Generic t, GGenDefault tag (Rep t)) => GenDefault tag (ViaGeneric tag t) where
  genDefault = fmap (ViaGeneric . to) . ggenDefault