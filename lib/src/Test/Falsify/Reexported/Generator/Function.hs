module Test.Falsify.Reexported.Generator.Function (
    Fun -- opaque
  , applyFun
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
    -- * Generation
  , fun
    -- * Construction
  , Function(..)
  , (:->) -- opaque
  , functionMap
  ) where

import Prelude hiding (sum)

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Foldable (toList)
import Data.Int
import Data.Kind
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ratio (Ratio)
import Data.Word
import GHC.Generics
import Numeric.Natural

import qualified Data.Ratio as Ratio

import Data.Falsify.Tree (Tree, Interval(..), Endpoint(..))
import Test.Falsify.Internal.Generator (Gen)
import Test.Falsify.Reexported.Generator.Shrinking
import Test.Falsify.Reexported.Generator.Compound

import qualified Data.Falsify.Tree as Tree

{-------------------------------------------------------------------------------
  Functions that can be shrunk and shown
-------------------------------------------------------------------------------}

-- | Function @a -> b@ which can be shown, generated, and shrunk
data Fun a b = Fun {
      concrete      :: a :-> b
    , defaultValue  :: b

      -- Since functions are typically infinite, they can only safely be shown
      -- once they are fully shrunk: after all, once a function has been fully
      -- shrunk, we /know/ it must be finite, because in any given property, a
      -- function will only ever be applied a finite number of times.
    , isFullyShrunk :: Bool
    }
  deriving (Functor)

-- | Generate function @a -> b@ given a generator for @b@
fun :: Function a => Gen b -> Gen (Fun a b)
fun gen = do
    -- Generate value first, so that we try to shrink that first
    defaultValue  <- gen
    concrete      <- function gen
    isFullyShrunk <- firstThen False True
    return Fun{concrete, defaultValue, isFullyShrunk}

{-------------------------------------------------------------------------------
  Concrete functions

  NOTE: @Nil@ is useful as a separate constructor, since it does not have an
  @Eq@ constraint.
-------------------------------------------------------------------------------}

data (:->) :: Type -> Type -> Type where
  Nil   :: a :-> b
  Unit  :: a -> () :-> a
  Table :: Ord a => Tree (a, Maybe b) -> a :-> b
  Sum   :: (a :-> c) -> (b :-> c) -> (Either a b :-> c)
  Prod  :: (a :-> (b :-> c)) -> (a, b) :-> c
  Map   :: (b -> a) -> (a -> b) -> (a :-> c) -> (b :-> c)

instance Functor ((:->) a) where
  fmap _ Nil           = Nil
  fmap f (Unit x)      = Unit (f x)
  fmap f (Table xs)    = Table (fmap (second (fmap f)) xs)
  fmap f (Sum x y)     = Sum (fmap f x) (fmap f y)
  fmap f (Prod x)      = Prod (fmap (fmap f) x)
  fmap f (Map ab ba x) = Map ab ba (fmap f x)

-- | The basic building block for 'Function' instances
--
-- Provides a 'Function' instance by mapping to and from a type that
-- already has a 'Function' instance.
functionMap :: (b -> a) -> (a -> b) -> (a :-> c) -> b :-> c
functionMap = Map

-- | Apply concrete function
abstract :: (a :-> b) -> b -> (a -> b)
abstract Nil         d _     = d
abstract (Unit x)    _ _     = x
abstract (Prod p)    d (x,y) = abstract (fmap (\q -> abstract q d y) p) d x
abstract (Sum p q)   d exy   = either (abstract p d) (abstract q d) exy
abstract (Table xys) d x     = fromMaybe d . join $ Tree.lookup x xys
abstract (Map g _ p) d x     = abstract p d (g x)

{-------------------------------------------------------------------------------
  Patterns

  These are analogue to their counterparts in QuickCheck.
-------------------------------------------------------------------------------}

-- | Pattern synonym useful when generating functions of one argument
pattern Fn :: (a -> b) -> Fun a b
pattern Fn f <- (applyFun -> f)

-- | Pattern synonym useful when generating functions of two arguments
pattern Fn2 :: (a -> b -> c) -> Fun (a, b) c
pattern Fn2 f <- (applyFun2 -> f)

-- | Pattern synonym useful when generating functions of three arguments
pattern Fn3 :: (a -> b -> c -> d) -> Fun (a, b, c) d
pattern Fn3 f <- (applyFun3 -> f)

-- | Apply function to argument
--
-- See also the 'Fn', 'Fn2', and 'Fn3' patter synonyms.
applyFun :: Fun a b -> a -> b
applyFun Fun{concrete, defaultValue} = abstract concrete defaultValue

applyFun2 :: Fun (a, b) c -> (a -> b -> c)
applyFun2 f a b = applyFun f (a, b)

applyFun3 :: Fun (a, b, c) d -> (a -> b -> c -> d)
applyFun3 f a b c = applyFun f (a, b, c)

{-# COMPLETE Fn  #-}
{-# COMPLETE Fn2 #-}
{-# COMPLETE Fn3 #-}

{-------------------------------------------------------------------------------
  Constructing concrete functions
-------------------------------------------------------------------------------}

shrinkToNil :: Gen (a :-> b) -> Gen (a :-> b)
shrinkToNil gen = fromMaybe Nil <$> shrinkToNothing gen

table :: forall a b. (Integral a, Bounded a) => Gen b -> Gen (a :-> b)
table gen = Table <$> bst (\_a -> shrinkToNothing gen) i
  where
    i :: Interval a
    i = Interval (Inclusive minBound) (Inclusive maxBound)

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
  Show functions
-------------------------------------------------------------------------------}

instance (Show a, Show b) => Show (Fun a b) where
  show Fun{concrete, defaultValue, isFullyShrunk}
    | isFullyShrunk = showFunction concrete defaultValue
    | otherwise     = "<fun>"

-- | Show concrete function
--
-- Only use this on finite functions.
showFunction :: (Show a, Show b) => (a :-> b) -> b -> String
showFunction p d = concat [
      "{"
    , intercalate ", " $ concat [
          [ show x ++ "->" ++ show c
          | (x,c) <- toTable p
          ]
        , ["_->" ++ show d]
        ]
    , "}"
    ]

-- | Generating a table from a concrete function
--
-- This is only used in the 'Show' instance.
toTable :: (a :-> b) -> [(a, b)]
toTable Nil         = []
toTable (Unit x)    = [((), x)]
toTable (Prod p)    = [ ((x,y),c) | (x,q) <- toTable p, (y,c) <- toTable q ]
toTable (Sum p q)   = [ (Left x, c) | (x,c) <- toTable p ]
                   ++ [ (Right y,c) | (y,c) <- toTable q ]
toTable (Table xys) = mapMaybe (\(a, b) -> (a,) <$> b) $ toList xys
toTable (Map _ h p) = [ (h x, c) | (x,c) <- toTable p ]

{-------------------------------------------------------------------------------
  Class to construct functions
-------------------------------------------------------------------------------}

-- | Generating functions
class Function a where
  -- | Build reified function
  --
  -- '(:->)' is an abstract type; if you need to add additional 'Function'
  -- instances, you need to use 'functionMap', or rely on the default
  -- implementation in terms of generics.
  function :: Gen b -> Gen (a :-> b)

  default function :: (Generic a, GFunction (Rep a)) => Gen b -> Gen (a :-> b)
  function gen = functionMap from to <$> gFunction gen

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
  function = fmap (functionMap toPair fromPair) . function
    where
      toPair :: Ratio a -> (a, a)
      toPair r = (Ratio.numerator r, Ratio.denominator r)

      fromPair :: (a, a) -> Ratio a
      fromPair (n, d) = n Ratio.% d

instance Function Char where
  function = fmap (functionMap ord chr) . function

-- instances that depend on generics

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
  Support for numbers
-------------------------------------------------------------------------------}

integral :: Integral a => Gen b -> Gen (a :-> b)
integral =
      fmap (functionMap
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
realFrac = fmap (functionMap toRational fromRational) . function

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

class GFunction f where
  gFunction :: Gen b -> Gen (f p :-> b)

instance GFunction f => GFunction (M1 i c f) where
  gFunction = fmap (functionMap unM1 M1) . gFunction @f

instance GFunction U1 where
  gFunction = fmap (functionMap unwrap wrap) . unit
    where
      unwrap :: U1 p -> ()
      unwrap _ = ()

      wrap :: () -> U1 p
      wrap _ = U1

instance (GFunction f, GFunction g) => GFunction (f :*: g) where
  gFunction = fmap (functionMap unwrap wrap) . prod (gFunction @f) (gFunction @g)
    where
      unwrap :: (f :*: g) p -> (f p, g p)
      unwrap (x :*: y) = (x, y)

      wrap :: (f p, g p) -> (f :*: g) p
      wrap (x, y) = x :*: y

instance (GFunction f, GFunction g) => GFunction (f :+: g) where
  gFunction =
      fmap (functionMap unwrap wrap) . sum (gFunction @f) (gFunction @g)
    where
      unwrap :: (f :+: g) p -> Either (f p) (g p)
      unwrap (L1 x) = Left  x
      unwrap (R1 y) = Right y

      wrap :: Either (f p) (g p) -> (f :+: g) p
      wrap (Left  x) = L1 x
      wrap (Right y) = R1 y

instance Function a => GFunction (K1 i a) where
  gFunction = fmap (functionMap unK1 K1) . function @a
