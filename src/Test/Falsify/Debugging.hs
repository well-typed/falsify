-- | Utilities for debugging
--
-- This is primarily useful for debugging/developing @falsify@ itself, although
-- some users might find these functions useful for understanding how their
-- generators work.
--
-- Intended for unqualified import.
module Test.Falsify.Debugging (
    -- * Shrinking
    ShrinkExplanation(..)
  , IsValidShrink(..)
  , limitShrinkSteps
  , shrinkHistory
    -- * Convenience driver-like functions
  , run
  , shrink
  , shrink'
    -- * Specialized generators
  , bindWithoutShortcut
  ) where

import Control.Monad
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)

import Test.Falsify.Internal.Generator.Definition
import Test.Falsify.Internal.Generator.Shrinking
import Test.Falsify.SampleTree (SampleTree(..), Sample, pattern Inf)

{-------------------------------------------------------------------------------
  Convenience driver-like functions
-------------------------------------------------------------------------------}

-- | Run generator against specified sample tree
run :: Gen a -> SampleTree -> a
run gen = fst . runGen gen

-- | Run the generator and return the full shrink history
--
-- Returns the empty list of the generator fails.
shrink :: (a -> Bool) -> Gen a -> SampleTree -> [a]
shrink prop gen =
      fromMaybe []
    . fmap (toList . shrinkHistory)
    . shrink' prop gen

-- | Run the generator and return the full shrink explanation.
--
-- Returns 'Nothing' if the initial value produced by the generator does not
-- satisfy the property.
shrink' :: forall a.
     (a -> Bool)
  -> Gen a
  -> SampleTree
  -> Maybe (ShrinkExplanation a a)
shrink' prop gen = aux . runGen gen
  where
    aux :: (a, [SampleTree]) -> Maybe (ShrinkExplanation a a)
    aux (outcome, shrunk) = do
        guard (prop outcome)
        return $ shrinkFrom prop' gen (outcome, shrunk)

    prop' :: a -> IsValidShrink a a
    prop' x = if prop x then ValidShrink x else InvalidShrink x

{-------------------------------------------------------------------------------
  Specialized generators
-------------------------------------------------------------------------------}

-- | Varation on @(>>=)@ that doesn't apply the shortcut to 'Minimal'
bindWithoutShortcut :: Gen a -> (a -> Gen b) -> Gen b
bindWithoutShortcut x f = Gen $ \(Inf s l r) ->
    let (a, ls) = runGen x l
        (b, rs) = runGen (f a) r
    in (b, combine s (l :| ls) (r :| rs))
  where
    -- Variation on 'combineShrunk' that doesn't apply the shortcut
    combine ::
         Sample
      -> NonEmpty SampleTree -- ^ Original and shrunk left  trees
      -> NonEmpty SampleTree -- ^ Original and shrunk right trees
      -> [SampleTree]
    combine s (l :| ls) (r :| rs) = concat [
          [SampleTree s l' r  | l' <- ls]
        , [SampleTree s l  r' | r' <- rs]
        ]

