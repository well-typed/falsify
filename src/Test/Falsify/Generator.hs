-- | Generator
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Generator (Gen)
-- > import qualified Test.Falsify.Generator qualified as Gen
module Test.Falsify.Generator (
    -- * Definition
    Gen -- opaque
  , runGen
    -- * Simple (non-compound) generators
  , bool
  , integral
  , enum
    -- ** Auxiliary
  , Precision(..)
  , integerWithPrecision
  , integerFromFraction
    -- * Compound generators
    -- ** Taking advantage of 'Selective'
  , choose
    -- ** Lists
  , list
  , elem
    -- *** Shuffling
  , shuffle
  , permutation
    -- ** Trees
    -- *** Binary trees
  , tree
  , bst
    -- *** Rose trees
  , RoseTree
  , path
  , pathAny
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
  ) where

import Prelude hiding (either, elem)

import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Reexported.Generator.Compound
import Test.Falsify.Reexported.Generator.Function
import Test.Falsify.Reexported.Generator.Simple
