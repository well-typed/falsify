{-# LANGUAGE UndecidableInstances #-}

-- | Default generators
--
-- 'GenDefault' (as well as t'Test.Falsify.GenDefault.Std.Std') are exported from
-- "Test.Falsify", so you will only need to import this module if you want to
-- make use of the deriving-via support.
--
-- Intended for unqualified import.
module Test.Falsify.GenDefault (
    GenDefault(..)
    -- * Deriving-via support
  , ViaTag(..)
  , ViaIntegral(..)
  , ViaEnum(..)
  , ViaList(..)
  , ViaString(..)
  , ViaGeneric(..)
  , GGenDefault -- opaque
  ) where

import Data.Bits (FiniteBits)
import Data.Proxy
import GHC.Exts (IsList (..), IsString (..))
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal, Nat)

import qualified Control.Applicative as Ap

import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range

-- | Default generators
--
-- 'GenDefault' is similar to QuickCheck's 'Test.QuickCheck.Arbitrary' class
-- along with some @deriving via@ helpers. Unlike @Arbitrary@, 'GenDefault'
-- allows one to choose between sets of default generators with user-defined
-- tags. See "Test.Falsify.GenDefault.Std" for the standard tag with a few
-- useful instances.
class GenDefault tag a where
  -- | Default generator for @a@
  --
  -- The type-level @tag@ allows types @a@ to have multiple defaults.
  genDefault :: Proxy tag -> Gen a

-- | DerivingVia wrapper for types with default instances under other tags
newtype ViaTag tag' a = ViaTag {unViaTag :: a}

instance GenDefault tag' a => GenDefault tag (ViaTag tag' a) where
  genDefault _ = fmap ViaTag (genDefault @tag' Proxy)

{-------------------------------------------------------------------------------
  Deriving-via helpers for types of specific shape
-------------------------------------------------------------------------------}

-- | DerivingVia wrapper for Integral types
newtype ViaIntegral a = ViaIntegral {unViaIntegral :: a}

instance (Integral a, FiniteBits a, Bounded a) => GenDefault tag (ViaIntegral a) where
  genDefault _ = fmap ViaIntegral (Gen.inRange Range.uniform)

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
    in fmap (ViaList . fromList) (Gen.list (Range.inclusive (bn, bx)) (genDefault p))

-- | DerivingVia wrapper for FromString types
newtype ViaString s (mn :: Nat) (mx :: Nat) = ViaString {unViaString :: s}

instance (IsString s, GenDefault tag Char, KnownNat mn, KnownNat mx) => GenDefault tag (ViaString s mn mx) where
  genDefault p =
    let bn = fromInteger (natVal (Proxy @mn))
        bx = fromInteger (natVal (Proxy @mx))
    in fmap (ViaString . fromString) (Gen.list (Range.inclusive (bn, bx)) (genDefault p))

{-------------------------------------------------------------------------------
  Generics
-------------------------------------------------------------------------------}

-- | Generic generator construction
--
-- For use with t'ViaGeneric'.
class GGenDefault tag f where
  {-# MINIMAL #-}
  ggenDefault :: Proxy tag -> Gen (f a)
  ggenDefault _ = error "ggenDefault not implemented"

instance GGenDefault tag U1 where
  ggenDefault _ = pure U1

instance GGenDefault tag a => GGenDefault tag (M1 i c a) where
  ggenDefault = fmap M1 . ggenDefault

instance (GGenDefault tag a, GGenDefault tag b) => GGenDefault tag (a :*: b) where
  ggenDefault p = Ap.liftA2 (:*:) (ggenDefault p) (ggenDefault p)

instance (GGenDefault tag a, GGenDefault tag b) => GGenDefault tag (a :+: b) where
  ggenDefault p = Gen.choose (fmap L1 (ggenDefault p)) (fmap R1 (ggenDefault p))

instance GenDefault tag a => GGenDefault tag (K1 i a) where
  ggenDefault = fmap K1 . genDefault

-- | DerivingVia wrapper for Generic types
newtype ViaGeneric tag a = ViaGeneric {unViaGeneric :: a}

instance (Generic t, GGenDefault tag (Rep t)) => GenDefault tag (ViaGeneric tag t) where
  genDefault = fmap (ViaGeneric . to) . ggenDefault