-- | Marked elements
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Marked (Mark(..), Marked(..))
-- > import qualified Test.Falsify.Marked as Marked
module Test.Falsify.Marked (
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

-- | Should an element in a container be kept or dropped?
--
-- See also t'Marked'.
data Mark = Keep | Drop
  deriving stock (Show, Eq, Ord)

-- | Marked element in a container
--
-- Marking elements can be a useful technique for dropping elements from a
-- container.
--
-- * Locally marking elements to 'Drop' makes it possible to enforce some global
--   constraints about minimum number of required elements before /actually/
--   dropping them (see 'selectAllKept').
--   Example: 'Test.Falsify.Generator.list'.
--
-- * For containers where we cannot remove random elements, the marks can be
--   used for \"outwards propagation\": if /this/ element is dropped, then
--   /those/ elements must also be dropped.
--   Example: 'Test.Falsify.Generator.tree'.
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

-- | Count how many elements will be kept
countKept :: Foldable t => t (Marked f a) -> Word
countKept = fromIntegral . length . mapMaybe shouldKeep . toList

-- | The element /if/ it should be kept
shouldKeep :: Marked f a -> Maybe (f a)
shouldKeep (Marked Keep x) = Just x
shouldKeep (Marked Drop _) = Nothing
