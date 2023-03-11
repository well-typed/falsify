-- | A single shrink step
--
-- Intended for qualified import.
--
-- import Test.Falsify.Internal.Generator.ShrinkStep (Step)
-- import qualified Test.Falsify.Internal.Generator.ShrinkStep as Step

module Test.Falsify.Internal.Generator.ShrinkStep (
    Step -- opaque
  , step
    -- * Working with the 'SampleTree'
  , next
  , left
  , right
    -- * Shrink the 'SampleTree'
  , Shortcut
  , shortcutMinimal
  , sampleTree
  ) where

import Data.Bifunctor
import Data.Functor
import Data.Word

import Test.Falsify.Internal.Generator.Definition
import Test.Falsify.SampleTree (SampleTree(..), Sample(..))

import qualified Test.Falsify.SampleTree as SampleTree

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A single step during shrinking
--
-- We separately record ways in which the value can be shrunk that is
-- independent of the 'SampleTree'.
newtype Step a = Step { step :: SampleTree -> [(a, SampleTree)] }
  deriving (Functor, Semigroup, Monoid)

{-------------------------------------------------------------------------------
  Stepping parts of the sample tree
-------------------------------------------------------------------------------}

-- | Shrink the next sample
next :: (Sample -> [Word64]) -> Step Sample
next f = Step $ \case
    Minimal          -> []
    SampleTree s l r -> f s <&> \s' -> (Shrunk s', SampleTree (Shrunk s') l r)

-- | Apply a step at the left subtree
left :: Step a -> Step a
left (Step f) = Step $ \case
    Minimal          -> []
    SampleTree s l r -> f l <&> \(a, l') -> (a, SampleTree s l' r)

-- | Apply a step at the right subtree
right :: Step a -> Step a
right (Step f) = Step $ \case
    Minimal          -> []
    SampleTree s l r -> f r <&> \(a, r') -> (a, SampleTree s l r')

{-------------------------------------------------------------------------------
  Shrinking the 'SampleTree', guided by a generator

  This could be considered to be the core of the @falsify@ approach.
-------------------------------------------------------------------------------}

-- | Shortcut shrinking
--
-- A shortcut is a way to shrink "faster" than the normal generator-driven
-- shrinker would shrink. It is given the shrunk sample trees that regular
-- shrinking would compute, and it can modify this list as it sees fit. The
-- identity function is a valid shortcut, but only in the absence of infinite
-- generators (see 'shortcutMinimal').
type Shortcut = forall a. Gen a -> Step a -> Step a

-- | Introduce the 'Minimal' sample tree at any point
--
-- It is important to introduce 'Minimal' when dealing with infinite generators:
-- since these look at an infinite portion of the sample tree. It is important
-- that we cut off this portion by introducing 'Minimal' at some point, to
-- ensure that shrinking terminates. The precise point of /where/ we introduce
-- 'Minimal' will depend on the property being tested: on the condition that
-- that property only depends on a finite part of the generated value, there
-- /must/ be a part of the sample tree that is not relevant and hence can
-- successfully be replaced by 'Minimal' without affecting what is being tested.
--
-- It is /also/ important that we /only/ introduce 'Minimal' if we can shrink
-- at /all/. This guarantees that we respect 'withoutShrinking' annotations;
-- see 'shrinkWith' for additional discussion.
shortcutMinimal :: Shortcut
shortcutMinimal gen (Step f) = Step $ \st ->
    case f st of
      []     -> []
      shrunk -> (run gen Minimal, Minimal) : shrunk

-- | Shrinking the 'SampleTree', guided by a generator
--
-- This is not quite as clean as in the paper, because it does two things at
-- once: shrink the sample tree /and/ run the generator. This results in better
-- performance (we need the results of execution /anyway/ in the definitions for
-- 'Bind' and 'Select'), and ultimately cleaner code overall (at call sites).
sampleTree :: Shortcut -> Gen a -> Step a
sampleTree shortcut = go
  where
    -- Apply the shortcut at any point in the tree
    go :: Gen a -> Step a
    go gen = shortcut gen $ go' gen

    go' :: Gen a -> Step a

    -- The generator is independent of the tree: /no point/ shrinking
    go' (Pure _) = mempty

    -- Actual shrinking only happens for the primitive generator
    -- We cannot shrink if the value is already minimal.
    go' (Prim (P f g)) = g <$> next f

    -- For 'Bind' we shrink either the left or the right tree.
    -- As is usual, this introduces a left bias.
    go' (Bind x f) = Step $ \st -> concat [
          first (\a -> run (f a) (SampleTree.right st)) <$>
            step (left (go x)) st
        , let a = run x (SampleTree.left st)
          in step (right (go (f a))) st
        ]

    -- The case for 'Select' is similar except that that we shrink the right
    -- subtree /only/ if it used: this is the raison d'être of 'Select'.
    go' (Select e f) = Step $ \st -> concat [
          first (either (run f (SampleTree.right st)) id) <$>
            step (left (go e)) st
        , case run e (SampleTree.left st) of
            Left  a -> first ($ a) <$> step (right (go f)) st
            Right _ -> []
        ]
