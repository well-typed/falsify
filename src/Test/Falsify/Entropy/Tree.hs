-- | Entropy tree
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Entropy.Tree (EntropyTree(L, B))
-- > import qualified Test.Falsify.Entropy.Tree as ETree
module Test.Falsify.Entropy.Tree (
    -- * Definition
    EntropyTree(L, B)
    -- * Operations
  , flatten
  ) where

import Data.Foldable (toList)
import Data.Word

import Test.Falsify.Entropy (Entropy(..))
import Test.Falsify.Util.Tree (Tree)

import qualified Test.Falsify.Util.Tree as Tree

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype EntropyTree = EntropyTree (Tree [Word8])
  deriving newtype (Show)

{-------------------------------------------------------------------------------
  Pattern synonyms
-------------------------------------------------------------------------------}

isBranch :: EntropyTree -> Maybe (EntropyTree, EntropyTree)
isBranch (EntropyTree t) =
    case t of
      Tree.B l r -> Just $ (EntropyTree l, EntropyTree r)
      _otherwise -> Nothing

pattern L :: [Word8] -> EntropyTree
pattern L xs = EntropyTree (Tree.L xs)

pattern B :: EntropyTree -> EntropyTree -> EntropyTree
pattern B l r <- (isBranch -> Just (l, r))
  where
    B (EntropyTree l) (EntropyTree r) = EntropyTree (Tree.B l r)

{-# COMPLETE L, B #-}

{-------------------------------------------------------------------------------
  Operations
-------------------------------------------------------------------------------}

flatten :: EntropyTree -> Entropy
flatten (EntropyTree t) = Entropy $ concat (toList t)