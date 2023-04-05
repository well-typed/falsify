module Data.Falsify.Tree (
    Tree(Leaf, Branch)
    -- * Dealing with marks
  , propagate
  , genKept
  , keepAtLeast
    -- * Binary search trees
  , Interval(..)
  , Endpoint(..)
  , inclusiveBounds
  , lookup
    -- * Debugging
  , drawTree
  ) where

import Prelude hiding (drop, lookup)

import Control.Selective (Selective, ifS)
import Control.Monad.State
import GHC.Show

import qualified Data.Tree as Rose

import Data.Falsify.Marked

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Tree a =
    Leaf

    -- 'Branch_' caches the size of the tree
  | Branch_ {-# UNPACK #-} !Word a (Tree a) (Tree a)
  deriving stock (Eq, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  Tree stats
-------------------------------------------------------------------------------}

-- | Size of the tree
--
-- @O(1)@
size :: Tree a -> Word
size Leaf              = 0
size (Branch_ s _ _ _) = s

{-------------------------------------------------------------------------------
  Pattern synonyms that hide the size argument
-------------------------------------------------------------------------------}

viewBranch :: Tree a -> Maybe (a, Tree a, Tree a)
viewBranch Leaf              = Nothing
viewBranch (Branch_ _ x l r) = Just (x, l, r)

branch :: a -> Tree a -> Tree a -> Tree a
branch x l r = Branch_ (1 + size l + size r) x l r

pattern Branch :: a -> Tree a -> Tree a -> Tree a
pattern Branch x l r <- (viewBranch -> Just (x, l, r))
  where
    Branch = branch

{-# COMPLETE Leaf, Branch #-}

{-------------------------------------------------------------------------------
  'Show' instance that depends on the pattern synonyms
-------------------------------------------------------------------------------}

instance Show a => Show (Tree a) where
  showsPrec _ Leaf           = showString "Leaf"
  showsPrec a (Branch x l r) = showParen (a > appPrec) $
        showString "Branch "
      . showsPrec appPrec1 x
      . showSpace
      . showsPrec appPrec1 l
      . showSpace
      . showsPrec appPrec1 r

{-------------------------------------------------------------------------------
  Dealing with marks
-------------------------------------------------------------------------------}

-- | Propagate 'Drop' marker down the tree
--
-- This is useful in conjunction with 'genKept', which truncates entire
-- subtrees.
propagate :: Tree (Marked f a) -> Tree (Marked f a)
propagate = keep
  where
    keep :: Tree (Marked f a) -> Tree (Marked f a)
    keep Leaf                         = Leaf
    keep (Branch (Marked Keep x) l r) = Branch (Marked Keep x) (keep l) (keep r)
    keep (Branch (Marked Drop x) l r) = Branch (Marked Drop x) (drop l) (drop r)

    drop :: Tree (Marked f a) -> Tree (Marked f a)
    drop = fmap $ \(Marked _ x) -> Marked Drop x

-- | Generate those values we want to keep
--
-- Whenever we meet an element marked 'Drop', that entire subtree is dropped.
genKept :: forall f a. Selective f => Tree (Marked f a) -> f (Tree a)
genKept = go
  where
    go :: Tree (Marked f a) -> f (Tree a)
    go Leaf                      = pure Leaf
    go (Branch (Marked m g) l r) = ifS (pure $ m == Keep)
                                     (Branch <$> g <*> go l <*> go r)
                                     (pure Leaf)

-- | Change enough nodes currently marked as 'Drop' to 'Keep' to ensure at
-- least @n@ nodes are marked 'Keep'.
--
-- Precondition: any 'Drop' marks must have been propagated; see 'propagate'.
-- Postcondition: this property is preserved.
keepAtLeast :: Word -> Tree (Marked f a) -> Tree (Marked f a)
keepAtLeast = \n t ->
    let kept = countKept t
    in if kept >= n
         then t
         else evalState (go t) (n - kept)
  where
    go :: Tree (Marked f a) -> State Word (Tree (Marked f a))
    go   Leaf                         = return Leaf
    go   (Branch (Marked Keep x) l r) = Branch (Marked Keep x) <$> go l <*> go r
    go t@(Branch (Marked Drop x) l r) = get >>= \case
         0 ->
           -- Nothing left to drop
           return t
         n | size t <= n -> do
          -- We can keep the entire subtree
          put $ n - size t
          return $ fmap (Marked Keep . unmark) t
         n ->  do
          -- We cannot delete the entire subtree. In order to preserve the
          -- "drop property", we /must/ mark this node as 'Keep'
          put $ n - 1
          Branch (Marked Keep x) <$> go l <*> go r

{-------------------------------------------------------------------------------
  BST
-------------------------------------------------------------------------------}

data Endpoint a = Inclusive a | Exclusive a
data Interval a = Interval (Endpoint a) (Endpoint a)

-- | Compute interval with inclusive bounds, without exceeding range
--
-- Returns 'Nothing' if the interval is empty, and @Just@ the inclusive
-- lower and upper bound otherwise.
inclusiveBounds :: forall a. (Ord a, Enum a) => Interval a -> Maybe (a, a)
inclusiveBounds = \(Interval lo hi) -> go lo hi
  where
    -- The inequality checks in @go@ justify the use of @pred@ or @succ@
    go :: Endpoint a -> Endpoint a -> Maybe (a, a)
    go (Inclusive lo) (Inclusive hi)
      | lo <= hi  = Just (lo, hi)
      | otherwise = Nothing
    go (Exclusive lo) (Inclusive hi)
      | lo < hi   = Just (succ lo, hi)
      | otherwise = Nothing
    go (Inclusive lo) (Exclusive hi)
      | lo < hi   = Just (lo, pred hi)
      | otherwise = Nothing
    go (Exclusive lo) (Exclusive hi)
      | lo < hi   = if succ lo > pred hi
                      then Nothing
                      else Just (succ lo, pred hi)
      | otherwise = Nothing


-- | Look value up in BST
--
-- NOTE: The 'Tree' datatype itself does /NOT/ guarantee that the tree is in
-- fact a BST. It is the responsibility of the caller to ensure this.
lookup :: Ord a => a -> Tree (a, b) -> Maybe b
lookup a' (Branch (a, b) l r)
  | a' < a    = lookup a' l
  | a' > a    = lookup a' r
  | otherwise = Just b
lookup _ Leaf = Nothing

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

drawTree :: Tree String -> String
drawTree = Rose.drawTree . conv
  where
    conv :: Tree String -> Rose.Tree String
    conv Leaf           = Rose.Node "*" []
    conv (Branch x l r) = Rose.Node x [conv l, conv r]
