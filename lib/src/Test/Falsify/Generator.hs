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
  , pick
  , pickBiased
  , shuffle
    -- ** Permutations
  , Permutation
  , applyPermutation
  , permutation
    -- ** Tweak test data distribution
  , frequency
    -- ** Trees
  , Tree(Leaf, Branch)
  , drawTree
    -- *** Binary trees
  , tree
  , bst
    -- *** Shrink trees
  , ShrinkTree
  , IsValidShrink(..)
  , path
  , pathAny
    -- ** Marking
  , Marked(..)
  , Mark(..)
  , selectAllKept
  , mark
    -- * Functions
    -- ** Generation
  , Fun
  , applyFun
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
  , fun
    -- ** Construction
  , Function(..)
  , (:->) -- opaque
  , functionMap
    -- * Reducing precision
  , WordN(..)
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
    -- * Generator independence
  , bindIntegral
  , perturb
    -- * Low-level
  , prim
  , primWith
  , exhaustive
  , captureLocalTree
  , bindWithoutShortcut
  ) where

import Prelude hiding (either, elem, properFraction)

import Data.Falsify.List
import Data.Falsify.Marked
import Test.Falsify.Internal.Generator
import Test.Falsify.Reexported.Generator.Compound
import Test.Falsify.Reexported.Generator.Function
import Test.Falsify.Reexported.Generator.Precision
import Test.Falsify.Reexported.Generator.Shrinking
import Test.Falsify.Reexported.Generator.Simple
import Data.Falsify.Tree
