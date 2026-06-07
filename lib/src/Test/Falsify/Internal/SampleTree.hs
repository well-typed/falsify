-- | Utilities for working with the sample tree
--
-- Intended for qualified import.
--
-- > import qualified Test.Falsify.Internal.SampleTree as SampleTree
module Test.Falsify.Internal.SampleTree (
    -- * Lenses
    next
  , left
  , right
  ) where

import Prelude hiding (map, mod)

import Optics.Core (Lens')

import qualified Optics.Core as Optics

import Test.Falsify.SampleTree (SampleTree(..), pattern Inf, Sample(..))

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
