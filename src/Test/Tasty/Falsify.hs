-- | Support for @falsify@ in the @tasty@ framework
--
-- As is customary, this also re-exports parts of the @falsify@ API, but not
-- modules such as "Test.Falsify.Range" that are intended to be imported
-- qualified.
module Test.Tasty.Falsify (
    -- * Test property
    testProperty
  , testShrinking
    -- * Configure test behaviour
  , TestOptions(..)
  , Verbose(..)
  , ExpectFailure(..)
  , testPropertyWith
  , testShrinkingWith
    -- * Re-exports
    -- ** Generators
  , Gen
    -- ** Properties
  , Property -- opaque
  , gen
  , assert
  , info
  ) where


import Test.Falsify.Generator (Gen)
import Test.Falsify.Internal.Tasty
import Test.Falsify.Property
