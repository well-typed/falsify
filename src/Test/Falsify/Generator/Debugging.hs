-- | Utilities for debugging generators
--
-- Intended for unqualified import.
module Test.Falsify.Generator.Debugging (
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
  ) where

import Test.Falsify.Internal.Generator.Definition
import Test.Falsify.Internal.Generator.Shrinking
import Test.Falsify.Internal.Generator.Truncated

