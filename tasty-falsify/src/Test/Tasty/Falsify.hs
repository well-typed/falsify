-- | Support for @falsify@ in the @tasty@ framework
module Test.Tasty.Falsify (
    testProperty
    -- * Override options
  , testPropertyWith
  , TestOptions(..)
  , Falsify.ExpectFailure(..)
  , Falsify.Verbose(..)
  ) where

import Test.Tasty.Falsify.Options
import Data.Default

import qualified Test.Falsify         as Falsify
import qualified Test.Falsify.Driver  as Falsify
import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.Providers as Tasty

import Test.Tasty.Falsify.Test

{-------------------------------------------------------------------------------
  User API
-------------------------------------------------------------------------------}

-- | Specialization of 'testPropertyWith' using default options
testProperty ::
     Tasty.TestName
  -> Falsify.Property ()
  -> Tasty.TestTree
testProperty = testPropertyWith def

-- | Test @falsify@ 'Falsify.Property' as part of a @tasty@ 'Tasty.TestTree'
testPropertyWith ::
     TestOptions
  -> Tasty.TestName
  -> Falsify.Property ()
  -> Tasty.TestTree
testPropertyWith testOpts name = Tasty.singleTest name . Test testOpts
