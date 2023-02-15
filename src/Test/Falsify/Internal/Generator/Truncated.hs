module Test.Falsify.Internal.Generator.Truncated (
    -- * Truncated sample tree
    Truncated(..)
  , expandTruncated
  , replaceValues
    -- * Generalized
  , Truncated'(..)
  , toTruncated'
  , expandTruncated'
  ) where

import Control.Monad.State
import Data.Set (Set)
import GHC.Stack

import qualified Data.Set as Set

import Test.Falsify.SampleTree (SampleTree(..), Sample(..))

{-------------------------------------------------------------------------------
  Truncated sample tree
-------------------------------------------------------------------------------}

-- | Version of 'SampleTree' where parts of the tree are omitted
--
-- Any particular run of a generator will only look at parts of the sample tree;
-- 'Truncated' records precisely this part of the tree.
data Truncated =
    -- | The generator did not need the tree at all
    E

    -- | The generator took a sample from the tree
  | S Sample

    -- | The generator split (branched) the sample tree
  | B Truncated Truncated
  deriving stock (Show, Eq)

-- | Expand truncated into full tree
--
-- Any values not present will be left as undefined in the full tree. This is
-- useful to demonstrate that a particular generator really only looks at the
-- parts of the sample tree described by the truncated tree. If this is
-- undesirable, consider expanding to 'Truncated'' with 'toTruncated'' and
-- then calling 'expandTruncated''.
expandTruncated :: HasCallStack => Truncated -> SampleTree
expandTruncated = go
  where
    go :: Truncated -> SampleTree
    go (B l r) = SampleTree unused (go l) (go r)
    go (S s)   = SampleTree s unused unused
    go E       = Minimal

    unused :: forall a. a
    unused = error "expandTruncated: unused"

-- | In-order traversal, replacing values in the tree until the list runs out
replaceValues :: [Sample] -> Truncated -> Truncated
replaceValues = \vs t -> evalState (go t) vs
  where
    go :: Truncated -> State [Sample] Truncated
    go E       = return E
    go (S s)   = state $ \case
                   []    -> (S s, [])
                   s':ss -> (S s', ss)
    go (B l r) = B <$> go l <*> go r

{-------------------------------------------------------------------------------
  Generalize
-------------------------------------------------------------------------------}

-- | Generalization of 'Truncated'' which allows to merge 'Truncated' trees
-- arising from different generators.
--
-- This is occassionally useful when trying to understand the same tree might be
-- interpreted differently by different generators; this is mostly relevant in
-- the context of generators that use monadic bind to change their behaviour
-- depending on previously generated values.
data Truncated' =
    E'
  | S' (Set Sample)
  | B' Truncated' Truncated'
  | F' (Set Sample) Truncated' Truncated'
  deriving stock (Show, Eq)

instance Semigroup Truncated' where
  E'       <> t'          = t'
  S' s     <> S' s'       = S' (s <> s')
  S' s     <> B'    l' r' = F'  s              l'        r'
  S' s     <> F' s' l' r' = F' (s <> s')       l'        r'
  B'   l r <> B'    l' r' = B'           (l <> l') (r <> r')
  B'   l r <> F' s' l' r' = F'       s'  (l <> l') (r <> r')
  F' s l r <> F' s' l' r' = F' (s <> s') (l <> l') (r <> r')
  t        <> t'          = t' <> t

instance Monoid Truncated' where
  mempty = E'

toTruncated' :: Truncated -> Truncated'
toTruncated' = go
  where
    go :: Truncated -> Truncated'
    go E       = E'
    go (S s)   = S' (Set.singleton s)
    go (B l r) = B' (go l) (go r)

expandTruncated' :: (Set Sample -> Sample) -> Truncated' -> SampleTree
expandTruncated' pick = go
  where
    go :: Truncated' -> SampleTree
    go (B' l r)   = SampleTree (pick' Set.empty) (go l) (go r)
    go (S' s)     = SampleTree (pick' s) Minimal Minimal
    go E'         = Minimal
    go (F' s l r) = SampleTree (pick' s) (go l) (go r)

    pick' :: Set Sample -> Sample
    pick' samples = case Set.toList samples of
                      [s] -> s
                      _   -> pick samples

