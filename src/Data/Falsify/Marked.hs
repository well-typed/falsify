-- | Marked elements
--
-- Intended for qualified import.
module Data.Falsify.Marked (
    Marked(..)
    -- * Queries
  , forget
  , shouldKeep
  , countKept
  ) where

import Data.Foldable (toList)
import Data.Maybe (mapMaybe)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Marked a = Keep a | Drop a
  deriving stock (Show, Eq, Functor)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

forget :: Marked a -> a
forget (Keep x) = x
forget (Drop x) = x

shouldKeep :: Marked a -> Maybe a
shouldKeep (Keep a) = Just a
shouldKeep (Drop _) = Nothing

countKept :: Foldable t => t (Marked a) -> Word
countKept = fromIntegral . length . mapMaybe shouldKeep . toList