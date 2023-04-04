-- | Sample tree
--
-- Intended for qualified import.
--
-- import Test.Falsify.SampleTree (SampleTree(..))
-- import qualified Test.Falsify.SampleTree as SampleTree
module Test.Falsify.SampleTree (
    -- * Definition
    SampleTree(..)
  , Sample(..)
  , pattern Inf
  , sampleValue
    -- * Lenses
  , next
  , left
  , right
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
import Optics.Core (Lens')
import System.Random.SplitMix

import qualified Optics.Core as Optics

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
    SampleTree Sample SampleTree SampleTree

    -- | Minimal tree (0 everywhere)
    --
    -- This constructor allows us to represent an infinite tree in a finite way
    -- and, importantly, /recognize/ a tree that is minimal everywhere. This is
    -- necessary when shrinking in the context of generators that generate
    -- infinitely large values.
  | Minimal
  deriving (Show)

{-------------------------------------------------------------------------------
  Samples
-------------------------------------------------------------------------------}

-- | Sample
--
-- The samples in the 'SampleTree' record if they were the originally produced
-- sample, or whether they have been shrunk.
data Sample =
    NotShrunk Word64
  | Shrunk    Word64
  deriving (Show, Eq, Ord)

sampleValue :: Sample -> Word64
sampleValue (NotShrunk s) = s
sampleValue (Shrunk    s) = s

{-------------------------------------------------------------------------------
  Views
-------------------------------------------------------------------------------}

view :: SampleTree -> (Sample, SampleTree, SampleTree)
view Minimal            = (Shrunk 0, Minimal, Minimal)
view (SampleTree s l r) = (s, l, r)

-- | Pattern synonym for treating the sample tree as infinite
pattern Inf :: Sample -> SampleTree -> SampleTree -> SampleTree
pattern Inf s l r <- (view -> (s, l, r))

{-# COMPLETE Inf #-}

{-------------------------------------------------------------------------------
  Lenses

  NOTE: The setter part of these lenses leaves 'Minimal' sample tree unchanged.
-------------------------------------------------------------------------------}

next :: Lens' SampleTree Sample
next = Optics.lens getter setter
  where
    getter :: SampleTree -> Sample
    getter (Inf s _ _) = s

    setter :: SampleTree -> Sample -> SampleTree
    setter Minimal _            = Minimal
    setter (SampleTree _ l r) s = SampleTree s l r

left :: Lens' SampleTree SampleTree
left = Optics.lens getter setter
  where
    getter :: SampleTree -> SampleTree
    getter (Inf _ l _) = l

    setter :: SampleTree -> SampleTree -> SampleTree
    setter Minimal            _ = Minimal
    setter (SampleTree s _ r) l = SampleTree s l r

right :: Lens' SampleTree SampleTree
right = Optics.lens getter setter
  where
    getter :: SampleTree -> SampleTree
    getter (Inf _ _ r) = r

    setter :: SampleTree -> SampleTree -> SampleTree
    setter Minimal            _ = Minimal
    setter (SampleTree s l _) r = SampleTree s l r

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
        in SampleTree (NotShrunk n) (go l) (go r)

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
    go = SampleTree (NotShrunk s) go go

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
    go (SampleTree s l r) = SampleTree (mapSample s) (go l) (go r)
    go Minimal            = Minimal

    mapSample :: Sample -> Sample
    mapSample (NotShrunk s) = NotShrunk (f s)
    mapSample (Shrunk    s) = Shrunk    (f s)

-- | Apply @mod m@ at every sample in the tree
--
-- This is primarily useful for debugging.
mod :: Word64 -> SampleTree -> SampleTree
mod m = map (\s -> s `Prelude.mod` m)

