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
  , int
  , enum
    -- * Compound generators
    -- ** Taking advantage of 'Selective'
  , choose
    -- ** Lists
  , list
  , elem
    -- *** Shuffling
  , shuffle
  , permutation
    -- ** Tweak test data distribution
  , frequency
    -- ** Trees
    -- *** Binary trees
  , tree
  , bst
    -- *** Rose trees
  , ShrinkTree
  , IsValidShrink(..)
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
    -- * Reducing precision
  , Precision(..)
  , Fraction(..)
  , WordN(..)
  , wordN
  , fraction
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
  , exhaustive
  , captureLocalTree
    -- * Generator independence
  , bindIntegral
  , perturb
    -- * Combinators
  , withoutShrinking
  ) where

import Prelude hiding (either, elem)

import Test.Falsify.Internal.Generator
import Test.Falsify.Reexported.Generator.Auxiliary
import Test.Falsify.Reexported.Generator.Compound
import Test.Falsify.Reexported.Generator.Function
import Test.Falsify.Reexported.Generator.Simple
