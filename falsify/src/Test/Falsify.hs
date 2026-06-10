-- | Main entry point for @falsify@
--
-- This provides all common definitions required for writing @falsify@ tests
-- and are intended for unqualified import. Typical usage:
--
-- > import Test.Falsify
-- > import qualified Test.Falsify.Generator as Gen
-- > import qualified Test.Falsify.Predicate as P
-- > import qualified Test.Falsify.Range     as Range
--
-- We do not export anything from the @Data.Falsify.*@ module hierarchy to
-- avoid name space pollution.
module Test.Falsify (
    -- * Property
    Property' -- opaque
  , Property
    -- ** Generating values
  , Gen -- opaque
  , Range -- opaque
  , Property.gen
  , Property.genWith
    -- ** Predicates
  , Predicate -- opaque
  , (.$)
  , Property.assert
    -- ** Other 'Property' features
  , Property.testFailed
  , Property.discard
  , Property.label
  , Property.collect
  , Property.info
    -- ** Testing generators
  , Property.testMinimum
  , Property.testShrinking
  , Property.testShrinkingOfGen
  , Property.testGen
  , Property.testGen'
    -- ** Functions
  , Fun -- opaque
    -- *** Patterns
  , Fun.applyFun
  , Fun.applyFun2
  , Fun.applyFun3
  , pattern Fun.Fn
  , pattern Fun.Fn2
  , pattern Fun.Fn3
    -- * Specialised data structures
    -- ** Marking
  , Marked(..)
  , Mark(..)
    -- ** Hedgehog and Quickcheck style shrinking
  , ShrinkTree(..)
    -- * Default generators
  , GenDefault(..)
  , Std
  ) where

import Test.Falsify.GenDefault
import Test.Falsify.GenDefault.Std
import Test.Falsify.Internal.Fun (Fun)
import Test.Falsify.Internal.Generator (Gen)
import Test.Falsify.Internal.Property (Property', Property)
import Test.Falsify.Internal.Range (Range)
import Test.Falsify.Marked (Mark(..), Marked(..))
import Test.Falsify.Predicate (Predicate, (.$))
import Test.Falsify.ShrinkTree (ShrinkTree(..))

import qualified Test.Falsify.Internal.Fun      as Fun
import qualified Test.Falsify.Internal.Property as Property
