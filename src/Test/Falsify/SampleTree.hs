-- | Sample tree
--
-- Intended for qualified import.
--
-- import Test.Falsify.SampleTree (SampleTree(..))
-- import qualified Test.Falsify.SampleTree as SampleTree
module Test.Falsify.SampleTree (
    -- * Definition
    SampleTree(..)
    -- * Views
  , next
  , split
    -- * Construction
  , fromPRNG
  , fromSeed
  , minimal
  , constant
    -- * Combinators
  , map
  , mod
  ) where

import Prelude hiding (map, mod)
import qualified Prelude

import Data.Word
import System.Random.SplitMix

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Sample tree
--
-- A sample tree is a (conceptually and sometimes actually) infinite tree
-- representing drawing values from and splitting a PRNG.
data SampleTree =
    -- | Default constructor
    --
    -- The type of ST is really
    --
    -- > ST :: Word64 & (SampleTree * SampleTree) -> SampleTree
    --
    -- where `(&)` is the additive conjunction from linear logic. In other
    -- words, the intention is that /either/ the @Word64@ is used, /or/
    -- the pair of subtrees; put another way, we /either/ draw a value from the
    -- PRNG, /or/ split it into two new PRNGs. See 'next' and 'split'.
    SampleTree Word64 SampleTree SampleTree

    -- | Minimal tree (0 everywhere)
    --
    -- This constructor allows us to represent an infinite tree in a finite way
    -- and, importantly, /recognize/ a tree that is minimal everywhere. This is
    -- necessary when shrinking in the context of generators that generate
    -- infinitely large values.
  | Minimal
  deriving (Show)

{-------------------------------------------------------------------------------
  Views
-------------------------------------------------------------------------------}

next :: SampleTree -> Word64
next (SampleTree s _ _) = s
next Minimal            = 0

split :: SampleTree -> (SampleTree, SampleTree)
split (SampleTree _ l r) = (l, r)
split Minimal            = (Minimal, Minimal)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromPRNG :: SMGen -> SampleTree
fromPRNG = go
  where
    go :: SMGen -> SampleTree
    go g =
        let (n, _) = nextWord64 g
            (l, r) = splitSMGen g
        in SampleTree n (go l) (go r)

fromSeed :: Word64 -> SampleTree
fromSeed = fromPRNG . mkSMGen

-- | Minimal sample tree
--
-- Generators should produce the \"simplest\" value when given this tree,
-- for some suitable application-specific definition of \"simple\".
minimal :: SampleTree
minimal = Minimal

-- | Sample tree that is the given value everywhere
--
-- This is primarily useful for debugging.
constant :: Word64 -> SampleTree
constant s = go
  where
    go :: SampleTree
    go = SampleTree s go go

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Map function over all random samples in the tree
--
-- Precondition: the function must preserve zeros:
--
-- > f 0 == 0
--
-- This means that we have
--
-- > map f M == M
--
-- This is primarily useful for debugging.
map :: (Word64 -> Word64) -> SampleTree -> SampleTree
map f = go
  where
    go :: SampleTree -> SampleTree
    go (SampleTree s l r) = SampleTree (f s) (go l) (go r)
    go Minimal            = Minimal

-- | Apply @mod m@ at every sample in the tree
--
-- This is primarily useful for debugging.
mod :: Word64 -> SampleTree -> SampleTree
mod m = map (\s -> s `Prelude.mod` m)

