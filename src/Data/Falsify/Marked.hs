-- | Marked elements
--
-- Intended for qualified import.
module Data.Falsify.Marked (
    Marked(..)
    -- * Queries
  , forget
  , shouldKeep
  , shouldDrop
    -- * Dealing with marks
  , keepAtLeast
  , countKept
  ) where

import Control.Monad.State
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

shouldDrop :: Marked a -> Maybe a
shouldDrop (Keep _) = Nothing
shouldDrop (Drop a) = Just a

{-------------------------------------------------------------------------------
  Dealing with marks
-------------------------------------------------------------------------------}

-- | Remark elements such that at least @n@ elements are marked 'Keep'
--
-- NOTE: This function is polymorphic in any traversable functor @t@; it can
-- mark individual elements, but it cannot preserve any datatype specific
-- invariants. For example, see also 'Data.Falsify.Tree.keepAtLeast'.
keepAtLeast :: forall t a.
     Traversable t
  => Word -> t (Marked a) -> t (Marked a)
keepAtLeast = \n xs ->
    let kept = countKept xs
    in if kept >= n
         then xs
         else evalState (traverse remark xs) (n - kept)
  where
    remark :: Marked a -> State Word (Marked a)
    remark (Keep x) = return $ Keep x
    remark (Drop x) = state $ \m -> if m == 0
                                      then (Drop x, 0)
                                      else (Keep x, m - 1)

-- | Count how many elements have been marked as 'Keep'
countKept :: Foldable t => t (Marked a) -> Word
countKept = fromIntegral . length . mapMaybe shouldKeep . toList