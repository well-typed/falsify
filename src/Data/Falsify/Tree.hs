module Data.Falsify.Tree (
    Tree(Leaf, Branch)
    -- * Stats
  , size
  , weight
  , height
    -- * Construction
  , singleton
    -- * Balancing
  , isWeightBalanced
  , isHeightBalanced
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
  , draw
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
  | Branch_ {-# UNPACK #-} !Stats a (Tree a) (Tree a)
  deriving stock (Eq, Functor, Foldable, Traversable)

data Stats = Stats {
      size_   :: {-# UNPACK #-} !Word
    , height_ :: {-# UNPACK #-} !Word
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Tree stats
-------------------------------------------------------------------------------}

-- | Size of the tree
--
-- @O(1)@
size :: Tree a -> Word
size Leaf              = 0
size (Branch_ s _ _ _) = size_ s

-- | Weight of the tree
--
-- The weight of a tree is simply its size plus one.
--
-- @O(1)@
weight :: Tree a -> Word
weight = succ . size

-- | Height of the tree
--
-- The height of a tree is the maximum length from the root to any of the leafs.
--
-- @O(1)@
height :: Tree a -> Word
height Leaf              = 0
height (Branch_ s _ _ _) = height_ s

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

singleton :: a -> Tree a
singleton x = Branch x Leaf Leaf

{-------------------------------------------------------------------------------
  Pattern synonyms that hide the size argument
-------------------------------------------------------------------------------}

viewBranch :: Tree a -> Maybe (a, Tree a, Tree a)
viewBranch Leaf              = Nothing
viewBranch (Branch_ _ x l r) = Just (x, l, r)

branch :: a -> Tree a -> Tree a -> Tree a
branch x l r = Branch_ s x l r
  where
    s :: Stats
    s = Stats {
          size_   = 1 + (size   l   +   size   r)
        , height_ = 1 + (height l `max` height r)
        }

pattern Branch :: a -> Tree a -> Tree a -> Tree a
pattern Branch x l r <- (viewBranch -> Just (x, l, r))
  where
    Branch = branch

{-# COMPLETE Leaf, Branch #-}

{-------------------------------------------------------------------------------
  'Show' instance that depends on the pattern synonyms
-------------------------------------------------------------------------------}

-- deriving instance Show a => Show (Tree a)

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
  Balancing
-------------------------------------------------------------------------------}

-- | Check if the tree is weight-balanced
--
-- A tree is weight-balanced if the weights of the subtrees does not differ
-- by more than a factor 3.
--
-- See "Balancing weight-balanced trees", Hirai and Yamamoto, JFP 21(3), 2011.
isWeightBalanced :: Tree a -> Bool
isWeightBalanced = checkBalanceCondition isBalanced
  where
    delta :: Word
    delta = 3

    isBalanced :: Tree a -> Tree a -> Bool
    isBalanced a b = and [
          delta * weight a >= weight b
        , delta * weight b >= weight a
        ]

-- | Check if a tree is height-balanced
--
-- A tree is height balanced if the heights of its subtrees do not differ
-- by more than one.
isHeightBalanced :: Tree a -> Bool
isHeightBalanced = checkBalanceCondition isBalanced
  where
    isBalanced :: Tree a -> Tree a -> Bool
    isBalanced a b = or [
          (height a <= height b) && (height b - height a <= 1)
        , (height b <= height a) && (height a - height b <= 1)
        ]

-- | Internal auxiliary: check given tree balance condition
--
-- Property @p l r@ will be checked at every branch in the tree.
checkBalanceCondition :: forall a. (Tree a -> Tree a -> Bool) -> Tree a -> Bool
checkBalanceCondition p = go
  where
    go :: Tree a -> Bool
    go Leaf           = True
    go (Branch _ l r) = and [p l r, go l, go r]

{-------------------------------------------------------------------------------
  Dealing with marks
-------------------------------------------------------------------------------}

-- | Propagate 'Drop' marker down the tree
--
-- This is useful in conjunction with 'genKept', which truncates entire
-- subtrees.
propagate :: Tree (Marked a) -> Tree (Marked a)
propagate = keep
  where
    keep :: Tree (Marked a) -> Tree (Marked a)
    keep Leaf                         = Leaf
    keep (Branch (Marked Keep x) l r) = Branch (Marked Keep x) (keep l) (keep r)
    keep (Branch (Marked Drop x) l r) = Branch (Marked Drop x) (drop l) (drop r)

    drop :: Tree (Marked a) -> Tree (Marked a)
    drop = fmap $ \(Marked _ x) -> Marked Drop x

-- | Generate those values we want to keep
--
-- Whenever we meet an element marked 'Drop', that entire subtree is dropped.
genKept :: forall f a. Selective f => Tree (Marked (f a)) -> f (Tree a)
genKept = go
  where
    go :: Tree (Marked (f a)) -> f (Tree a)
    go Leaf                      = pure Leaf
    go (Branch (Marked m g) l r) = ifS (pure $ m == Keep)
                                     (Branch <$> g <*> go l <*> go r)
                                     (pure Leaf)

-- | Change enough nodes currently marked as 'Drop' to 'Keep' to ensure at
-- least @n@ nodes are marked 'Keep'.
--
-- Precondition: any 'Drop' marks must have been propagated; see 'propagate'.
-- Postcondition: this property is preserved.
keepAtLeast :: Word -> Tree (Marked a) -> Tree (Marked a)
keepAtLeast = \n t ->
    let kept = countKept t
    in if kept >= n
         then t
         else evalState (go t) (n - kept)
  where
    go :: Tree (Marked a) -> State Word (Tree (Marked a))
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

draw :: Tree String -> String
draw = Rose.drawTree . conv
  where
    conv :: Tree String -> Rose.Tree String
    conv Leaf           = Rose.Node "*" []
    conv (Branch x l r) = Rose.Node x [conv l, conv r]
