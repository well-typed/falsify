-- | Generator
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Generator (Gen)
-- > import qualified Test.Falsify.Generator qualified as Gen
module Test.Falsify.Generator (
    -- * Definition
    Gen -- opaque
    -- * Simple (non-compound) generators
  , bool
  , integral
  , enum
    -- ** Auxiliary
  , Precision(..)
  , integerWithPrecision
  , integerFromFraction
    -- * Compound generators
    -- ** Lists
  , list
  , elem
    -- ** Trees
    -- *** Binary trees
  , tree
  , bst
    -- *** Rose trees
  , RoseTree
  , path
    -- ** Auxiliary
  , shrinkToNothing
  , mark
    -- * Functions
  , Fun
  , applyFun
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
  , fun
    -- * User-specified shrinking
  , shrinkToOneOf
  , firstThen
  , shrinkWith
    -- * Shrink trees
  , fromShrinkTree
  , toShrinkTree
    -- * Primitive
  , prim
  , primWith
  , captureLocalTree
    -- * Combinators
  , withoutShrinking
    -- * Execution
  , run
  , shrink
  , shrinkWithShortcut
    -- * Re-exports
  , Alt(..)
  ) where

import Prelude hiding (either, elem)

import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Reexported.Generator.Compound
import Test.Falsify.Reexported.Generator.Function
import Test.Falsify.Reexported.Generator.Instances
import Test.Falsify.Reexported.Generator.Simple
