-- | Properties
--
-- Intended for unqualified import.
--
-- Most users will probably use "Test.Tasty.Falsify" instead of this module.
module Test.Falsify.Property (
    Property -- opaque
  , gen
  , assert
  , assertBool
  , info
  ) where

import Test.Falsify.Internal.Property
