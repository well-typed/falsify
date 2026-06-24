-- | Generator
--
-- Intended for qualified import.
--
-- > import Test.Falsify
-- > import qualified Test.Falsify.Generator as Gen
module Test.Falsify.Generator (
    Gen -- opaque
    -- * Simple (non-compound) generators
  , bool
  , inRange
  , int
    -- * Compound generators
    -- ** Taking advantage of 'Control.Selective.Selective'
  , choose
  , oneof
    -- ** Lists
  , list
  , elem
  , pick
  , pickBiased
  , shuffle
    -- ** Permutations
  , permutation
    -- ** Tweak test data distribution
  , frequency
    -- ** Trees
  , tree
  , bst
    -- ** Shrink trees
  , path
  , pathAny
    -- ** Marking
  , mark
    -- * Functions
  , fun
  , Function(..)
  , GFunction -- opaque
    -- * Reducing precision
  , wordN
  , properFraction
    -- * Overriding shrinking
  , withoutShrinking
  , shrinkToOneOf
  , firstThen
  , shrinkWith
  , shrinkToNothing
    -- * Shrink trees
  , fromShrinkTree
  , toShrinkTree
  , toShrinkTreeWithContext
    -- * Generator independence
  , bindIntegral
  , perturb
    -- * Low-level
  , prim
  , primWith
  , exhaustive
  , captureLocalTree
  , bindWithoutShortcut
  , minimalValue
  ) where

import Prelude hiding (either, elem, properFraction)

import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Generator.Compound
import Test.Falsify.Internal.Generator.Function
import Test.Falsify.Internal.Generator.Precision
import Test.Falsify.Internal.Generator.Shrinking
import Test.Falsify.Internal.Generator.Simple
