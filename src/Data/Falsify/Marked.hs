-- | Marked elements
--
-- Intended for unqualified import.
module Data.Falsify.Marked (
    Mark(..)
  , Marked(..)
    -- * Generation
  , selectKept
    -- * Queries
  , countKept
  , shouldKeep
  ) where

import Control.Selective
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Mark = Keep | Drop
  deriving stock (Show, Eq, Ord)

data Marked a = Marked {
      getMark :: Mark
    , unmark  :: a
    }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

selectKept :: Selective f => Marked (f a) -> f (Maybe a)
selectKept (Marked mark gen) =
    ifS (pure $ mark == Keep)
        (Just <$> gen)
        (pure Nothing)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

countKept :: Foldable t => t (Marked a) -> Word
countKept = fromIntegral . length . mapMaybe shouldKeep . toList

shouldKeep :: Marked a -> Maybe a
shouldKeep (Marked Keep x) = Just x
shouldKeep (Marked Drop _) = Nothing
