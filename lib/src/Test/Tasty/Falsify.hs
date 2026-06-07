-- | Support for @falsify@ in the @tasty@ framework
--
-- As is customary, this also re-exports parts of the @falsify@ API, but not
-- modules such as "Test.Falsify.Range" that are intended to be imported
-- qualified.
module Test.Tasty.Falsify (
    -- * Test property
    testProperty
    -- * Configure test behaviour
  , TestOptions(..)
  , Verbose(..)
  , ExpectFailure(..)
  , testPropertyWith
    -- * Re-exports
  , module Test.Falsify.Property
    -- ** Generators
  , Gen
    -- ** Functions
  , applyFun
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
  ) where

import Test.Falsify.Fun
import Test.Falsify.Generator (Gen)
import Test.Falsify.Internal.Driver.Tasty
import Test.Falsify.Property
