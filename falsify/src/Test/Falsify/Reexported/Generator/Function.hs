module Test.Falsify.Reexported.Generator.Function (
    fun
  , Function(..)
  , GFunction -- opaque
  ) where

import Prelude hiding (sum)

import Data.Char
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Ratio (Ratio)
import Data.Void (Void)
import Data.Word
import GHC.Generics
import Numeric.Natural

import qualified Data.Ratio as Ratio

import Data.Falsify.ConcreteFun ((:->)(..))
import Test.Falsify.Internal.Fun
import Test.Falsify.Internal.Generator (Gen)
import Test.Falsify.Reexported.Generator.Compound
import Test.Falsify.Reexported.Generator.Shrinking

import qualified Data.Falsify.ConcreteFun as ConcreteFun

{-------------------------------------------------------------------------------
  Functions that can be shrunk and shown
-------------------------------------------------------------------------------}

-- | Generate function @a -> b@ given a generator for @b@
fun :: Function a => Gen b -> Gen (Fun a b)
fun gen = do
    -- Generate value first, so that we try to shrink that first
    defaultValue  <- gen
    concrete      <- function gen
    isFullyShrunk <- firstThen False True
    return Fun{concrete, defaultValue, isFullyShrunk}

{-------------------------------------------------------------------------------
  Constructing concrete functions
-------------------------------------------------------------------------------}

shrinkToNil :: Gen (a :-> b) -> Gen (a :-> b)
shrinkToNil gen = fromMaybe Nil <$> shrinkToNothing gen

table :: forall a b. (Integral a, Bounded a) => Gen b -> Gen (a :-> b)
table gen = Table <$> bst (\_a -> shrinkToNothing gen) (minBound, maxBound)

unit :: Gen c -> Gen (() :-> c)
unit gen = shrinkToNil (Unit <$> gen)

sum ::
     (Gen c -> Gen (       a   :-> c))
  -> (Gen c -> Gen (         b :-> c))
  -> (Gen c -> Gen (Either a b :-> c))
sum f g gen = Sum <$> shrinkToNil (f gen) <*> shrinkToNil (g gen)

prod ::
     (forall c. Gen c -> Gen ( a     :-> c))
  -> (forall c. Gen c -> Gen (    b  :-> c))
  -> (forall c. Gen c -> Gen ((a, b) :-> c))
prod f g = fmap Prod . f . g

{-------------------------------------------------------------------------------
  Class to construct functions
-------------------------------------------------------------------------------}

-- | Generating functions
class Function a where
  -- | Build reified function
  --
  -- If you need to add additional 'Function' instances, you will typically
  -- define them using 'Data.Falsify.Concrete.map', or rely on the default
  -- implementation in terms of generics.
  function :: Gen b -> Gen (a :-> b)

  default function :: (Generic a, GFunction (Rep a)) => Gen b -> Gen (a :-> b)
  function gen = ConcreteFun.map from to <$> gFunction gen

instance Function Word8 where function = table
instance Function Int8  where function = table

instance Function Int     where function = integral
instance Function Int16   where function = integral
instance Function Int32   where function = integral
instance Function Int64   where function = integral
instance Function Word    where function = integral
instance Function Word16  where function = integral
instance Function Word32  where function = integral
instance Function Word64  where function = integral
instance Function Integer where function = integral
instance Function Natural where function = integral

instance Function Float  where function = realFrac
instance Function Double where function = realFrac

instance (Integral a, Function a) => Function (Ratio a) where
  function = fmap (ConcreteFun.map toPair fromPair) . function
    where
      toPair :: Ratio a -> (a, a)
      toPair r = (Ratio.numerator r, Ratio.denominator r)

      fromPair :: (a, a) -> Ratio a
      fromPair (n, d) = n Ratio.% d

instance Function Char where
  function = fmap (ConcreteFun.map ord chr) . function

-- instances that depend on generics

instance Function ()
instance Function Bool
instance Function Void

instance (Function a, Function b) => Function (Either a b)

instance Function a => Function [a]
instance Function a => Function (Maybe a)

-- Tuples (these are also using generics)

-- 2
instance
     ( Function a
     , Function b
     )
  => Function (a, b)

-- 3
instance
     ( Function a
     , Function b
     , Function c
     )
  => Function (a, b, c)

-- 4
instance
     ( Function a
     , Function b
     , Function c
     , Function d
     )
  => Function (a, b, c, d)

-- 5
instance
     ( Function a
     , Function b
     , Function c
     , Function d
     , Function e
     )
  => Function (a, b, c, d, e)

-- 6
instance
     ( Function a
     , Function b
     , Function c
     , Function d
     , Function e
     , Function f
     )
  => Function (a, b, c, d, e, f)

-- 7
instance
     ( Function a
     , Function b
     , Function c
     , Function d
     , Function e
     , Function f
     , Function g
     )
  => Function (a, b, c, d, e, f, g)

{-------------------------------------------------------------------------------
  Support for numbers
-------------------------------------------------------------------------------}

integral :: Integral a => Gen b -> Gen (a :-> b)
integral =
      fmap (ConcreteFun.map
             (fmap bytes  . toSignedNatural   . toInteger)
             (fromInteger . fromSignedNatural . fmap unbytes)
           )
    . function
  where
    bytes :: Natural -> [Word8]
    bytes 0 = []
    bytes n = fromIntegral (n `mod` 256) : bytes (n `div` 256)

    unbytes :: [Word8] -> Natural
    unbytes []     = 0
    unbytes (w:ws) = fromIntegral w + 256 * unbytes ws

realFrac :: RealFrac a => Gen b -> Gen (a :-> b)
realFrac = fmap (ConcreteFun.map toRational fromRational) . function

data Signed a = Pos a | Neg a
  deriving stock (Show, Functor, Generic)
  deriving anyclass (Function)

toSignedNatural :: Integer -> Signed Natural
toSignedNatural n
  | n < 0     = Neg (fromInteger (abs n - 1))
  | otherwise = Pos (fromInteger n)

fromSignedNatural :: Signed Natural -> Integer
fromSignedNatural (Neg n) = negate (toInteger n + 1)
fromSignedNatural (Pos n) = toInteger n

{-------------------------------------------------------------------------------
  Generic support for 'Function'
-------------------------------------------------------------------------------}

-- | Generic construction of concrete functions
--
-- See 'Function' for discussion.
class GFunction f where
  {-# MINIMAL #-}
  gFunction :: Gen b -> Gen (f p :-> b)
  gFunction = error "gFunction not implemented"

instance GFunction f => GFunction (M1 i c f) where
  gFunction = fmap (ConcreteFun.map unM1 M1) . gFunction @f

instance GFunction V1 where
  gFunction _ = pure Nil

instance GFunction U1 where
  gFunction = fmap (ConcreteFun.map unwrap wrap) . unit
    where
      unwrap :: U1 p -> ()
      unwrap _ = ()

      wrap :: () -> U1 p
      wrap _ = U1

instance (GFunction f, GFunction g) => GFunction (f :*: g) where
  gFunction = fmap (ConcreteFun.map unwrap wrap) . prod (gFunction @f) (gFunction @g)
    where
      unwrap :: (f :*: g) p -> (f p, g p)
      unwrap (x :*: y) = (x, y)

      wrap :: (f p, g p) -> (f :*: g) p
      wrap (x, y) = x :*: y

instance (GFunction f, GFunction g) => GFunction (f :+: g) where
  gFunction =
      fmap (ConcreteFun.map unwrap wrap) . sum (gFunction @f) (gFunction @g)
    where
      unwrap :: (f :+: g) p -> Either (f p) (g p)
      unwrap (L1 x) = Left  x
      unwrap (R1 y) = Right y

      wrap :: Either (f p) (g p) -> (f :+: g) p
      wrap (Left  x) = L1 x
      wrap (Right y) = R1 y

instance Function a => GFunction (K1 i a) where
  gFunction = fmap (ConcreteFun.map unK1 K1) . function @a
