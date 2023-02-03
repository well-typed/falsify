module Test.Falsify.Internal.Generator.Shrinking (
    -- * Shrinking
    shrink
  , shrinkStep
    -- * Debugging
  , ShrinkExplanation(..)
  , shrinkExplain
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Stack

import qualified Data.List.NonEmpty as NE

import Test.Falsify.Internal.Generator.Definition
import Test.Falsify.Internal.Generator.Truncated
import Test.Falsify.SampleTree (SampleTree(..))

{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

-- | Find smallest value that the generator can produce and still satisfies
-- the predicate.
--
-- Returns the full shrink history.
--
-- Precondition: in @shrink p g st@, we should have
--
-- > p (run g st)
--
-- In other words, the predicate should hold for the initial sample tree.
shrink :: forall a.
     HasCallStack
  => (a -> Bool) -- ^ Predicate to check if something is a valid shrink
  -> Gen a
  -> SampleTree
  -> NonEmpty a
shrink p g = fmap snd . history . shrinkExplain p g

-- | Explanation of how shrinking proceeded
data ShrinkExplanation a = ShrinkExplanation {
      -- | All successful shrink steps
      --
      -- For each step, we record the value produced by the generator, as well
      -- as the part of the sample tree that the generator used.
      history :: NonEmpty (Truncated, a)

      -- | The candidates that were rejected
      --
      -- If this list is empty, shrinking failed because the sample tree could
      -- not be shrunk further. If not, then all of these represent possible
      -- next shrink steps that however all failed the supplied predicate (i.e.,
      -- typically these represent values that are no longer counter-examples to
      -- whatever property is being tested).
      --
      -- Note that the list here is different from the interpretation of the
      -- list in 'history': in 'history', the list corresponds to a series of
      -- successful shrink steps
      --
      -- > x0 ~> x1 ~> .. ~> xN
      --
      -- The list of 'rejected' next steps are all possible next steps from @xN@,
      -- all of which fail the predicate:
      --
      -- > history                   rejected
      -- > ----------------------------------
      -- >                         / y0
      -- >                         | y1
      -- > x0 ~> x1 ~> .. ~> xN ~> | ..
      -- >                         | ..
      -- >                         \ yM
    , rejected :: [(Truncated, a)]
    }
  deriving (Show)

-- | Generalization of 'shrink' which explains the process
--
-- This is occassionally useful when debugging a generator, for example when it
-- is shrinking in unexpected ways.
shrinkExplain :: forall a.
     HasCallStack
  => (a -> Bool) -> Gen a -> SampleTree -> ShrinkExplanation a
shrinkExplain p g = \st ->
    let unshrunk = runExplain g st in
    if not (p $ snd unshrunk)
      then error "shrink: precondition violated"
      else go (unshrunk :| []) st
  where
    go :: NonEmpty (Truncated, a) -> SampleTree -> ShrinkExplanation a
    go acc st =
        -- Shrinking is a greedy algorithm: we go with the first candidate that
        -- works, and discard the others.
        case filter (p . snd . snd) candidates of
          []        -> ShrinkExplanation (NE.reverse acc) (map snd candidates)
          (st',c):_ -> go (NE.cons c acc) st'
      where
        candidates :: [(SampleTree, (Truncated, a))]
        candidates = map (\st' -> (st', runExplain g st')) $ shrinkStep g st

-- | Single step in shrinking
--
-- This is an auxiliary function used in shrinking; users will typically never
-- have to call this function.
shrinkStep :: Gen a -> SampleTree -> [SampleTree]
shrinkStep = go
  where
    go :: Gen a -> SampleTree -> [SampleTree]

    -- Tree is already minimal: /cannot/ shrink any further
    go _ Minimal = []

    -- The generator is independent of the tree: /no point/ shrinking
    go (Pure _) _ = []

    -- Actual shrinking only happens for the primitive generator
    -- We cannot shrink if the value is already minimal.
    go (Prim f _) (SampleTree s l r) = (\s' -> SampleTree s' l r) <$> f s

    -- Finally, for 'Bind' we shrink either the left or the right tree; as is
    -- usual, this introduces a left bias.
    go (Bind x f) (SampleTree s l r) = shortcut . concat $ [
          (\l' -> SampleTree s l' r)  <$> go x             l
        , (\r' -> SampleTree s l  r') <$> go (f $ run x l) r
        ]

    -- In the 'Bind' case, if we can shrink at all, we also try to shrink to
    -- 'Minimal' directly. This is important for dealing with generators of
    -- infinite data structures, which might otherwise shrink indefinitely.
    shortcut :: [SampleTree] -> [SampleTree]
    shortcut [] = []
    shortcut ts = Minimal : ts
