-- | Binary trees
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Util.Tree (Tree(..))
-- > import qualified Test.Falsify.Util.Tree as Tree
module Test.Falsify.Util.Tree (
    Tree(..)
  ) where

-- | Binary tree
data Tree a = L a | B (Tree a) (Tree a)
  deriving (Show, Functor, Foldable)