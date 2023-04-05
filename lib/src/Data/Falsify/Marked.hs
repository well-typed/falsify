-- | Marked elements
--
-- Intended for unqualified import.
module Data.Falsify.Marked (
    Mark(..)
  , Marked(..)
    -- * Generation
  , selectAllKept
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

data Marked f a = Marked {
      getMark :: Mark
    , unmark  :: f a
    }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

selectKept :: Selective f => Marked f a -> f (Maybe a)
selectKept (Marked mark gen) =
    ifS (pure $ mark == Keep)
        (Just <$> gen)
        (pure Nothing)

-- | Traverse the argument, generating all values marked 'Keep', and replacing
-- all values marked 'Drop' by 'Nothing'
selectAllKept ::
     (Traversable t, Selective f)
  => t (Marked f a) -> f (t (Maybe a))
selectAllKept = traverse selectKept

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

countKept :: Foldable t => t (Marked f a) -> Word
countKept = fromIntegral . length . mapMaybe shouldKeep . toList

shouldKeep :: Marked f a -> Maybe (f a)
shouldKeep (Marked Keep x) = Just x
shouldKeep (Marked Drop _) = Nothing
