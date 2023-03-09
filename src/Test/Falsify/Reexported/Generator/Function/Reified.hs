module Test.Falsify.Reexported.Generator.Function.Reified (
    -- * Reified functions
    (:->)(..)
  , abstract
  , showFunction
    -- ** Smart constructors
  , mkProd
  , mkSum
  , mkTable
  , mkMap
    -- * Construction
  , Function(..)
  , functionMap
  , functionMapWith
  ) where

import Data.Bifunctor
import Data.Char
import Data.Int
import Data.List (intersperse)
import Data.Ratio (Ratio)
import Data.Word
import GHC.Generics
import Numeric.Natural

import qualified Data.Ratio as Ratio

import Test.Falsify.Reexported.Generator.Function.Perturb

{-------------------------------------------------------------------------------
  Reified functions

  This is adapted from code in in QuickCheck.
-------------------------------------------------------------------------------}

-- | Reified functions
--
-- The constructors of this type are not exported.
data a :-> c where
  Nil   :: a :-> c
  Unit  :: c -> (() :-> c)
  Prod  :: (a :-> (b :-> c)) -> ((a, b) :-> c)
  Sum   :: (a :-> c) -> (b :-> c) -> (Either a b :-> c)
  Table :: (Eq a, Perturb a) => [(a, c)] -> (a :-> c)
  Map   :: (a -> b) -> (b -> a) -> (b :-> c) -> (a :-> c)

instance Functor ((:->) a) where
  fmap _ Nil           = Nil
  fmap f (Unit c)      = Unit (f c)
  fmap f (Prod p)      = Prod (fmap (fmap f) p)
  fmap f (Sum p q)     = Sum (fmap f p) (fmap f q)
  fmap f (Table xys)   = Table $ map (second f) xys
  fmap f (Map ab ba p) = Map ab ba (fmap f p)

-- | Turn concrete function into an abstract function (with a default result)
abstract :: (a :-> c) -> c -> (a -> c)
abstract (Prod p)    d (x,y) = abstract (fmap (\q -> abstract q d y) p) d x
abstract (Sum p q)   d exy   = either (abstract p d) (abstract q d) exy
abstract (Unit c)    _ _     = c
abstract Nil         d _     = d
abstract (Table xys) d x     = head ([y | (x',y) <- xys, x == x'] ++ [d])
abstract (Map g _ p) d x     = abstract p d (g x)

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

instance (Show a, Show b) => Show (a :-> b) where
  show p = showFunction p Nothing

-- | Show reified function
--
-- Only use this on finite functions.
showFunction :: (Show a, Show b) => (a :-> b) -> Maybe b -> String
showFunction p md = concat [
      "{"
    , concat . intersperse ", " $ concat [
          [ show x ++ "->" ++ show c
          | (x,c) <- table p
          ]
        , [ "_->" ++ show d
          | Just d <- [md]
          ]
        ]
    , "}"
    ]

-- | Generating a table from a concrete function
--
-- This is only used in the 'Show' instance.
table :: (a :-> c) -> [(a,c)]
table (Prod p)    = [ ((x,y),c) | (x,q) <- table p, (y,c) <- table q ]
table (Sum p q)   = [ (Left x, c) | (x,c) <- table p ]
                 ++ [ (Right y,c) | (y,c) <- table q ]
table (Unit c)    = [ ((), c) ]
table Nil         = []
table (Table xys) = xys
table (Map _ h p) = [ (h x, c) | (x,c) <- table p ]

{-------------------------------------------------------------------------------
  Internal auxiliary: smart constructors
-------------------------------------------------------------------------------}

mkProd :: (a :-> (b :-> c)) -> (a, b) :-> c
mkProd Nil = Nil
mkProd p   = Prod p

mkSum :: (a :-> c) -> (b :-> c) -> Either a b :-> c
mkSum Nil Nil = Nil
mkSum p   q   = Sum p q

mkTable :: (Eq a, Perturb a) => [(a, c)] -> a :-> c
mkTable []  = Nil
mkTable xys = Table xys

mkMap :: (a -> b) -> (b -> a) -> (b :-> c) -> a :-> c
mkMap _ _ Nil = Nil
mkMap f g p   = Map f g p

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Functions that can be reified
--
-- This is analogue to the class of the same name in 'QuickCheck', but our
-- definition of reified functions (@:->@) is different.
class Function a where
  function :: (a -> b) -> (a :-> b)

  default function :: (Generic a, GFunction (Rep a)) => (a -> b) -> (a :-> b)
  function = genericFunction

-- "Primitive" instances

instance Function Int8  where function = functionBoundedEnum
instance Function Word8 where function = functionBoundedEnum

instance Function Int     where function = functionIntegral
instance Function Int16   where function = functionIntegral
instance Function Int32   where function = functionIntegral
instance Function Int64   where function = functionIntegral
instance Function Word    where function = functionIntegral
instance Function Word16  where function = functionIntegral
instance Function Word32  where function = functionIntegral
instance Function Word64  where function = functionIntegral
instance Function Integer where function = functionIntegral
instance Function Natural where function = functionIntegral

instance Function Float  where function = functionRealFrac
instance Function Double where function = functionRealFrac

-- Derived instances

instance Function Char where
  function = functionMap ord chr

instance (Integral a, Function a) => Function (Ratio a) where
  function = functionMap toPair fromPair
    where
      toPair :: Ratio a -> (a, a)
      toPair r = (Ratio.numerator r, Ratio.denominator r)

      fromPair :: (a, a) -> Ratio a
      fromPair (n, d) = n Ratio.% d

-- Instances that depend on generics

instance Function ()
instance Function Bool
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
  Additional constructors for 'Map'
-------------------------------------------------------------------------------}

-- | The basic building block for 'Function' instances
--
-- Provides a 'Function' instance by mapping to and from a type that
-- already has a 'Function' instance.
functionMap ::
     Function b
  => (a -> b) -> (b -> a) -> (a -> c) -> (a :-> c)
functionMap ab ba = functionMapWith ab ba function

-- | Convenience function for constructiong 'Map'
functionMapWith ::
     (a -> b)
  -> (b -> a)
  -> ((b -> c) -> (b :-> c))
  -> ((a -> c) -> (a :-> c))
functionMapWith ab ba f = mkMap ab ba . (f .  (. ba))

{-------------------------------------------------------------------------------
  Auxiliary functions for constructing 'Function' instances

  We cannot use deriving-via here because the role of the first parameter to
  @:->@ is nominal (due to to the constraints imposed by 'Table').
-------------------------------------------------------------------------------}

functionBoundedEnum ::
     (Eq a, Perturb a, Enum a, Bounded a)
  => (a -> b) -> a :-> b
functionBoundedEnum f = Table $ [ (x, f x) | x <- [minBound .. maxBound] ]

functionIntegral :: Integral a => (a -> b) -> a :-> b
functionIntegral =
    functionMap
      (fmap bytes  . toSignedNatural   . toInteger)
      (fromInteger . fromSignedNatural . fmap unbytes)
  where
    bytes :: Natural -> [Word8]
    bytes 0 = []
    bytes n = fromIntegral (n `mod` 256) : bytes (n `div` 256)

    unbytes :: [Word8] -> Natural
    unbytes []     = 0
    unbytes (w:ws) = fromIntegral w + 256 * unbytes ws

functionRealFrac :: RealFrac a => (a -> b) -> (a :-> b)
functionRealFrac = functionMap toRational fromRational

{-------------------------------------------------------------------------------
  Generic support for 'Function'
-------------------------------------------------------------------------------}

-- | Generic 'Function' implementation.
genericFunction :: (Generic a, GFunction (Rep a)) => (a -> b) -> (a :-> b)
genericFunction = functionMapWith from to gFunction

class GFunction f where
  gFunction :: (f a -> b) -> (f a :-> b)

instance GFunction f => GFunction (M1 i c f) where
  gFunction = functionMapWith unM1 M1 gFunction

instance GFunction U1 where
  gFunction = functionMapWith unwrap wrap $ \f ->
      Unit $ f ()
    where
      unwrap :: U1 p -> ()
      unwrap _ = ()

      wrap :: () -> U1 p
      wrap _ = U1

instance (GFunction f, GFunction g) => GFunction (f :*: g) where
  gFunction = functionMapWith unwrap wrap $ \f ->
      mkProd $ gFunction <$> gFunction (curry f)
    where
      unwrap :: (f :*: g) p -> (f p, g p)
      unwrap (x :*: y) = (x, y)

      wrap :: (f p, g p) -> (f :*: g) p
      wrap (x, y) = x :*: y

instance (GFunction f, GFunction g) => GFunction (f :+: g) where
  gFunction = functionMapWith unwrap wrap $ \f ->
      mkSum (gFunction (f . Left)) (gFunction (f . Right))
    where
      unwrap :: (f :+: g) p -> Either (f p) (g p)
      unwrap (L1 x) = Left  x
      unwrap (R1 y) = Right y

      wrap :: Either (f p) (g p) -> (f :+: g) p
      wrap (Left  x) = L1 x
      wrap (Right y) = R1 y

instance Function a => GFunction (K1 i a) where
  gFunction = functionMap unK1 K1

{-------------------------------------------------------------------------------
  Internal auxiliary: dealing with signed numbers

  Lemma:

  > forall n, fromSignedNatural (toSignedNatural n) == n

  Proof: case split on whether @n@ is negative or positive. If negative:

  >    fromSignedNatural (toSignedNatural n)
  > == fromSignedNatural (Neg (fromInteger (abs n - 1)))
  > == negate (toInteger (fromInteger (abs n - 1)) + 1)
  > == negate (abs n - 1 + 1)
  > == negate (abs n)
  > == n (if n negative)

  The case for positive is similar, but easier.
  -----------------------------------------------------------------------------}

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

