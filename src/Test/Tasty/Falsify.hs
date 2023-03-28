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
    -- ** Generators
  , Gen
    -- ** Properties
  , Property -- opaque
    -- *** Running generators
  , gen
  , genWith
    -- *** Additional 'Property' features
  , assert
  , assertBool
  , assertEqual
  , info
    -- *** Testing shrinking
  , testShrinking
    -- ** Functions
  , pattern Gen.Fn
  , pattern Gen.Fn2
  , pattern Gen.Fn3
  ) where

import Test.Falsify.Generator (Gen)
import Test.Falsify.Internal.Tasty
import Test.Falsify.Property

import qualified Test.Falsify.Reexported.Generator.Function as Gen
