-- | Properties
--
-- Intended for unqualified import.
--
-- Most users will probably use "Test.Tasty.Falsify" instead of this module.
module Test.Falsify.Property (
    Property -- opaque
    -- * Run generators
  , gen
  , genWith
    -- * Additional 'Property' features
  , assert
  , info
  , discard
  , discardIf
    -- * Test shrinking
  , genShrinkPath
  , testShrinking
  ) where

import Test.Falsify.Internal.Property
