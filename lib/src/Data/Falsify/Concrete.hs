-- | Concrete functions
--
-- Intended for qualified import.
--
-- > import Data.Falsify.Concrete ((:->)(..))
-- > import qualified Data.Falsify.Concrete as Concrete
module Data.Falsify.Concrete (
    (:->)(..)
    -- * Construction
  , map
    -- * Application
  , apply
    -- * Rendering
  , render
  ) where

import Prelude hiding (map)

import Control.Monad
import Data.Bifunctor
import Data.Foldable (toList)
import Data.Kind
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)

import Data.Falsify.Tree (Tree(..))

import qualified Data.Falsify.Tree as Tree

{-------------------------------------------------------------------------------
  Definition

  NOTE: @Nil@ is useful as a separate constructor, since it does not have an
  @Eq@ constraint.
-------------------------------------------------------------------------------}

-- | Concrete function
--
-- A concrete function is essentially a deep embedding: an explicit
-- representation of the ouput value of the function for every input value in
-- the function's domain.
--
-- Concrete functions are the central building block for generating random
-- functions, as proposed by Koen Claessen in \"Shrinking and showing
-- functions\", Haskell Symposium 2012
-- (<https://dl.acm.org/doi/10.1145/2430532.2364516>). Koen's key insight is
-- that we can start with an infinite concrete function that is defined on every
-- value in the input domain, but since in any given test that function is only
-- applied to a finite number of inputs, we can then shrink the concrete
-- function, throwing away entire chunks of the input domain, until we are left
-- with a finite representation which can be printed as part of a test output.
--
-- All of the cleverness for /generating/ concrete functions is about reducing
-- the explicitly represented /domain/ of the function. To shrink the /outputs/
-- of the function nothing special is needed, and we can just rely on Haskell's
-- laziness and the fact that @falsify@ is carefully constructed so that it can
-- generate infinite data types.
data (:->) :: Type -> Type -> Type where
  Nil   :: a :-> b
  Unit  :: a -> () :-> a
  Table :: Ord a => Tree (a, Maybe b) -> a :-> b
  Sum   :: (a :-> c) -> (b :-> c) -> (Either a b :-> c)
  Prod  :: (a :-> (b :-> c)) -> (a, b) :-> c
  Map   :: (b -> a) -> (a -> b) -> (a :-> c) -> (b :-> c)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

instance Functor ((:->) a) where
  fmap _ Nil           = Nil
  fmap f (Unit x)      = Unit (f x)
  fmap f (Table xs)    = Table (fmap (second (fmap f)) xs)
  fmap f (Sum x y)     = Sum (fmap f x) (fmap f y)
  fmap f (Prod x)      = Prod (fmap (fmap f) x)
  fmap f (Map ab ba x) = Map ab ba (fmap f x)

-- | Change domain of concrete function
--
-- This is the basic building block for constructing new concrete functions.
map :: (b -> a) -> (a -> b) -> (a :-> c) -> b :-> c
map = Map

{-------------------------------------------------------------------------------
  Application
-------------------------------------------------------------------------------}

-- | Apply concrete function
apply :: (a :-> b) -> b -> (a -> b)
apply Nil         d _     = d
apply (Unit x)    _ _     = x
apply (Prod p)    d (x,y) = apply (fmap (\q -> apply q d y) p) d x
apply (Sum p q)   d exy   = either (apply p d) (apply q d) exy
apply (Table xys) d x     = fromMaybe d . join $ Tree.lookup x xys
apply (Map g _ p) d x     = apply p d (g x)

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Show concrete function
--
-- Only use this on finite functions!
render :: (Show a, Show b) => (a :-> b) -> b -> String
render p d = concat [
      "{"
    , intercalate ", " $ concat [
          [ show x ++ "->" ++ show c
          | (x,c) <- toTable p
          ]
        , ["_->" ++ show d]
        ]
    , "}"
    ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Generating a table from a concrete function
--
-- Used in 'render'.
toTable :: (a :-> b) -> [(a, b)]
toTable Nil         = []
toTable (Unit x)    = [((), x)]
toTable (Prod p)    = [ ((x,y),c) | (x,q) <- toTable p, (y,c) <- toTable q ]
toTable (Sum p q)   = [ (Left x, c) | (x,c) <- toTable p ]
                   ++ [ (Right y,c) | (y,c) <- toTable q ]
toTable (Table xys) = mapMaybe (\(a, b) -> (a,) <$> b) $ toList xys
toTable (Map _ h p) = [ (h x, c) | (x,c) <- toTable p ]
