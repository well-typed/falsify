-- | Utilities for debugging
--
-- Intended for unqualified import.
module Test.Falsify.Debugging (
    -- * Truncated sample tree
    Truncated(..)
  , expandTruncated
  , replaceValues
    -- * Generalization
  , Truncated'(..)
  , toTruncated'
  , expandTruncated'
    -- * Running
  , runExplain
    -- * Shrinking
  , ShrinkExplanation(..)
  , shrinkExplain
  , shrinkStep
  , limitShrinkSteps
  , shrinkHistory
  ) where

import Test.Falsify.Internal.Generator.Definition
import Test.Falsify.Internal.Generator.Shrinking
import Test.Falsify.Internal.Generator.Truncated

