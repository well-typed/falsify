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
  , explainGen
    -- * Shrinking
  , ShrinkExplanation(..)
  , IsValidShrink(..)
  , shrinkExplain
  , shrinkStep
  , limitShrinkSteps
  , shrinkHistory
  , Shortcut
  , shortcutMinimal
  ) where

import Test.Falsify.Internal.Generator.Definition
import Test.Falsify.Internal.Generator.Shrinking
import Test.Falsify.Internal.Generator.Truncated

