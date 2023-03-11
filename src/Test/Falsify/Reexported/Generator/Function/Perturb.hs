module Test.Falsify.Reexported.Generator.Function.Perturb (
    -- * Focs
    Focus(..)
  , variant
    -- * Internal utilities
  , getAtFocus
  , stepAtFocus
   -- * Perturb
  , Perturb(..)
  ) where

import Data.Bits
import Data.Int
import Data.Ratio (Ratio)
import Data.Word
import GHC.Generics
import Numeric.Natural

import qualified Data.Ratio as Ratio

import Test.Falsify.SampleTree (SampleTree(..))
import Test.Falsify.Internal.Generator.ShrinkStep (Step)

import qualified Test.Falsify.Internal.Generator.ShrinkStep as Step
import qualified Test.Falsify.SampleTree                    as SampleTree

{-------------------------------------------------------------------------------
  Focus

  The implementation of 'variant' is an adaptation of @integerVariant@ in
  QuickCheck.
-------------------------------------------------------------------------------}

data Focus = FocusHere | FocusLeft Focus | FocusRight Focus

instance Semigroup Focus where
  FocusHere    <> f' = f'
  FocusLeft  f <> f' = FocusLeft  (f <> f')
  FocusRight f <> f' = FocusRight (f <> f')

instance Monoid Focus where
  mempty = FocusHere

-- | Mark variant
--
-- See 'Perturb'.
variant :: Integer -> Focus
variant = \n ->
    if n >= 1
      then gamma n       $ FocusLeft  FocusHere
      else gamma (1 - n) $ FocusRight FocusHere
  where
    gamma :: Integer -> Focus -> Focus
    gamma n = encode n (ilog2 n) . zeroes k
      where
        k = ilog2 n

    encode :: Integer -> Int -> Focus -> Focus
    encode n = go
      where
        go (-1) g = g
        go k g
          | testBit n k = go (k - 1) $ FocusRight g
          | otherwise   = go (k - 1) $ FocusLeft  g

    -- The zeroes are effectively an encoding of the "length" of the number,
    -- in terms of the number of bits required to encode it as a binary number.
    -- This is used to ensure that, say, variant @0b11@ doesn't get a subtree
    -- of the sample tree used for @0b1@.
    zeroes :: Int -> Focus -> Focus
    zeroes 0 g = g
    zeroes k g = zeroes (k - 1) $ FocusLeft  g

    ilog2 :: Integer -> Int
    ilog2 1 = 0
    ilog2 n = 1 + ilog2 (n `div` 2)

{-------------------------------------------------------------------------------
  Using 'Focus'

  This does not need to be part of the publicly exported API.
-------------------------------------------------------------------------------}

getAtFocus :: Focus -> SampleTree -> SampleTree
getAtFocus = go
  where
    go :: Focus -> SampleTree -> SampleTree
    go FocusHere      = id
    go (FocusLeft  f) = go f . SampleTree.left
    go (FocusRight f) = go f . SampleTree.right

stepAtFocus :: Focus -> Step a -> Step a
stepAtFocus = go
  where
    go :: Focus -> Step a -> Step a
    go FocusHere      = id
    go (FocusLeft  f) = Step.left  . go f
    go (FocusRight f) = Step.right . go f

{-------------------------------------------------------------------------------
  Perturbations
-------------------------------------------------------------------------------}

-- | Perturb the PRNG
--
-- This is the analogue of 'CoArbitrary' in QuickCheck.
class Perturb a where
  perturb :: a -> Focus

  default perturb :: (Generic a, GPerturb (Rep a)) => a -> Focus
  perturb = gPerturb . from

-- Instances that rely on deriving-via

deriving via AsEnum Bool     instance Perturb Bool
deriving via AsEnum Char     instance Perturb Char
deriving via AsEnum Ordering instance Perturb Ordering

deriving via AsIntegral Int     instance Perturb Int
deriving via AsIntegral Int8    instance Perturb Int8
deriving via AsIntegral Int16   instance Perturb Int16
deriving via AsIntegral Int32   instance Perturb Int32
deriving via AsIntegral Int64   instance Perturb Int64
deriving via AsIntegral Word    instance Perturb Word
deriving via AsIntegral Word8   instance Perturb Word8
deriving via AsIntegral Word16  instance Perturb Word16
deriving via AsIntegral Word32  instance Perturb Word32
deriving via AsIntegral Word64  instance Perturb Word64
deriving via AsIntegral Integer instance Perturb Integer
deriving via AsIntegral Natural instance Perturb Natural

deriving via AsReal Float  instance Perturb Float
deriving via AsReal Double instance Perturb Double

-- Instances that rely on generics

instance Perturb ()
instance Perturb a => Perturb [a]
instance (Perturb a, Perturb b) => Perturb (Either a b)

-- Tuples (also relies on generics)
--
-- We don't provide instances for larger than 7; this is an arbitrary of course,
-- but happens to match the limit for tuples that have a Generics instance.

-- 2
instance
     ( Perturb a
     , Perturb b
     )
  => Perturb (a, b)

-- 3
instance
     ( Perturb a
     , Perturb b
     , Perturb c
     )
  => Perturb (a, b, c)

-- 4
instance
     ( Perturb a
     , Perturb b
     , Perturb c
     , Perturb d
     )
  => Perturb (a, b, c, d)

-- 5
instance
     ( Perturb a
     , Perturb b
     , Perturb c
     , Perturb d
     , Perturb e
     )
  => Perturb (a, b, c, d, e)

-- 6
instance
     ( Perturb a
     , Perturb b
     , Perturb c
     , Perturb d
     , Perturb e
     , Perturb f
     )
  => Perturb (a, b, c, d, e, f)

-- 7
instance
     ( Perturb a
     , Perturb b
     , Perturb c
     , Perturb d
     , Perturb e
     , Perturb f
     , Perturb g
     )
  => Perturb (a, b, c, d, e, f, g)

{-------------------------------------------------------------------------------
  Generic support for 'Perturb'
-------------------------------------------------------------------------------}

class GPerturb f where
  gPerturb :: f a -> Focus

instance GPerturb f => GPerturb (M1 i c f) where
  gPerturb = gPerturb . unM1

instance GPerturb U1 where
  gPerturb U1 = FocusHere

instance (GPerturb f, GPerturb g) => GPerturb (f :*: g) where
  gPerturb (x :*: y) = gPerturb x <> gPerturb y

instance (GPerturb f, GPerturb g) => GPerturb (f :+: g) where
  gPerturb (L1 x) = variant 0 <> gPerturb x
  gPerturb (R1 y) = variant 1 <> gPerturb y

instance Perturb c => GPerturb (K1 i c) where
  gPerturb (K1 c) = perturb c

{-------------------------------------------------------------------------------
  Deriving-via support for 'Perturb'
-------------------------------------------------------------------------------}

newtype AsIntegral a = WrapAsIntegral a
newtype AsEnum     a = WrapAsEnum     a
newtype AsReal     a = WrapAsReal     a

instance Integral a => Perturb (AsIntegral a) where
  perturb (WrapAsIntegral x) = variant $ toInteger x

instance Enum a => Perturb (AsEnum a) where
  perturb (WrapAsEnum x) = variant $ fromIntegral (fromEnum x)

instance Real a => Perturb (AsReal a) where
  perturb (WrapAsReal x) =
      perturb (Ratio.numerator r, Ratio.denominator r)
    where
      r :: Ratio Integer
      r = toRational x
