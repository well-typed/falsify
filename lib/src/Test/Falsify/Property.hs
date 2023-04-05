-- | Properties
--
-- Intended for unqualified import.
--
-- Most users will probably use "Test.Tasty.Falsify" instead of this module.
module Test.Falsify.Property (
    Property' -- opaque
  , Property
    -- * Run generators
  , gen
  , genWith
    -- * 'Property' features
  , testFailed
  , assert
  , info
  , discard
  , label
  , collect
    -- * Testing shrinking
  , testShrinking
  , testMinimum
    -- * Testing generators
  , testGen
  , testShrinkingOfGen
  ) where

import Test.Falsify.Internal.Property

-- | Property that uses strings as errors
type Property = Property' String