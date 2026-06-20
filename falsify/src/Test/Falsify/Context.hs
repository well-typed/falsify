-- | Context
--
-- Intended for qualified import.
--
-- > import Test.Falsify
-- > import qualified Test.Falsify.Context as Context
module Test.Falsify.Context (
    Context.Context(..)
  , getContext
  ) where

import Test.Falsify.Internal.Property (getContext)

import qualified Test.Falsify.Internal.Context as Context
