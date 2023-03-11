module Test.Falsify.Internal.Generator.Shrinking (
    -- * Shrinking
    shrink
  , shrinkWithShortcut
    -- * With full history
  , ShrinkExplanation(..)
  , ShrinkHistory(..)
  , Candidate(..)
  , IsValidShrink(..)
  , shrinkExplain
  , limitShrinkSteps
  , shrinkHistory
  ) where

import Data.Bifunctor
import Data.Either
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Stack

import Test.Falsify.Internal.Generator.Definition
import Test.Falsify.SampleTree (SampleTree(..))

import qualified Test.Falsify.Internal.Generator.ShrinkStep as Step

{-------------------------------------------------------------------------------
  Explanation
-------------------------------------------------------------------------------}

-- | Shrink explanation
--
-- @p@ is the type of \"positive\" elements that satisfied the predicate (i.e.,
-- valid shrinks), and @n@ is the type of \"negative\" which didn't.
data ShrinkExplanation p n = ShrinkExplanation {
      -- | The value we started, before shrinking
      initial :: Candidate p

      -- | The full shrink history
    , history :: ShrinkHistory p n
    }

-- | Shrink explanation
data ShrinkHistory p n =
    -- | We successfully executed a single shrink step
    ShrunkTo (Candidate p) (ShrinkHistory p n)

    -- | We could no shrink any further
    --
    -- We also record all rejected next steps. This is occasionally useful when
    -- trying to figure out why a value didn't shrink any further (what did it
    -- try to shrink to?)
  | ShrinkingDone [Candidate n]

    -- | We stopped shrinking early
    --
    -- This is used when the number of shrink steps is limited.
  | ShrinkingStopped

limitShrinkSteps :: Maybe Word -> ShrinkExplanation p n -> ShrinkExplanation p n
limitShrinkSteps Nothing      = id
limitShrinkSteps (Just limit) = \case
    ShrinkExplanation{initial, history} ->
      ShrinkExplanation{
          initial
        , history = go limit history
        }
  where
    go :: Word -> ShrinkHistory p n -> ShrinkHistory p n
    go 0 (ShrunkTo _ _)      = ShrinkingStopped
    go n (ShrunkTo x xs)     = ShrunkTo x (go (pred n) xs)
    go _ (ShrinkingDone rej) = ShrinkingDone rej
    go _ ShrinkingStopped    = ShrinkingStopped

-- | Simplify the shrink explanation to keep only the shrink history
shrinkHistory :: ShrinkExplanation p n -> NonEmpty p
shrinkHistory (ShrinkExplanation unshrunk shrunk) =
    outcome unshrunk :| go shrunk
  where
    go :: ShrinkHistory p n -> [p]
    go (ShrunkTo x xs)   = outcome x : go xs
    go (ShrinkingDone _) = []
    go ShrinkingStopped  = []

{-------------------------------------------------------------------------------
  Mapping
-------------------------------------------------------------------------------}

instance Bifunctor ShrinkExplanation where
  bimap f g ShrinkExplanation{initial, history} = ShrinkExplanation{
        initial = fmap f initial
      , history = bimap f g history
      }

instance Bifunctor ShrinkHistory where
  bimap f g = \case
      ShrunkTo truncated history ->
        ShrunkTo (fmap f truncated) (bimap f g history)
      ShrinkingDone rejected ->
        ShrinkingDone (map (fmap g) rejected)
      ShrinkingStopped ->
        ShrinkingStopped

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
shrink = shrinkWithShortcut Step.shortcutMinimal

-- | Generalization of 'shrink'
shrinkWithShortcut :: forall a.
     HasCallStack
  => Step.Shortcut
  -> (a -> Bool) -- ^ Predicate to check if something is a valid shrink
  -> Gen a
  -> SampleTree
  -> NonEmpty a
shrinkWithShortcut shortcut p g =
    shrinkHistory . shrinkExplain shortcut p' g
  where
    p' :: a -> IsValidShrink a ()
    p' x = if p x then ValidShrink x else InvalidShrink ()

{-------------------------------------------------------------------------------
  Generalized shrinking interface
-------------------------------------------------------------------------------}

-- | Does a given shrunk value represent a valid shrink step?
data IsValidShrink p n =
    ValidShrink p
  | InvalidShrink n

-- | Generalization of 'shrink' which explains the process
--
-- This is occassionally useful when debugging a generator, for example when it
-- is shrinking in unexpected ways.
--
-- This function has a more precise type than 'shrink', which suffers from
-- boolean blindness; here we get /evidence/ whether or not something is a valid
-- shrink step.
--
-- This is lazy in the shrink history; see 'limitShrinkSteps' to limit the
-- number of shrinking steps.
shrinkExplain :: forall a p n.
     HasCallStack
  => Step.Shortcut
  -> (a -> IsValidShrink p n)
  -> Gen a -> SampleTree -> ShrinkExplanation p n
shrinkExplain shortcut p gen = \st ->
    case evalSampleTree p (run gen st, st) of
      Left  c -> ShrinkExplanation c $ go st
      Right _ -> error "shrink: precondition violated"
  where
    go :: SampleTree -> ShrinkHistory p n
    go st =
        -- Shrinking is a greedy algorithm: we go with the first candidate that
        -- works, and discard the others.
        --
        -- NOTE: 'partitionEithers' is lazy enough:
        --
        -- > head . fst $ partitionEithers [Left True, undefined] == True
        case partitionEithers candidates of
          ([], rejected) -> ShrinkingDone rejected
          (c:_, _)       -> ShrunkTo c $ go (shrunkTree c)
      where
        candidates :: [Either (Candidate p) (Candidate n)]
        candidates =
            map (evalSampleTree p) $
              Step.step (Step.sampleTree shortcut gen) st

{-------------------------------------------------------------------------------
  Shrinking candidates
-------------------------------------------------------------------------------}

-- | Candidate during shrink
data Candidate x = Candidate {
      -- | The shrunk 'SampleTree'
      shrunkTree :: SampleTree

      -- | The result of the generator
    , outcome :: x
    }
  deriving (Functor)

evalSampleTree ::
     (a -> IsValidShrink p n)
  -> (a, SampleTree)
  -> Either (Candidate p) (Candidate n)
evalSampleTree prop (a, shrunkTree) =
    case prop a of
      ValidShrink   p -> Left  $ Candidate{shrunkTree, outcome = p}
      InvalidShrink n -> Right $ Candidate{shrunkTree, outcome = n}
