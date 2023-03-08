module Test.Falsify.Internal.Generator.Shrinking (
    -- * Shrinking
    shrink
  , shrinkWithShortcut
  , Shortcut
  , shortcutMinimal
    -- * With full history
  , ShrinkExplanation(..)
  , ShrinkHistory(..)
  , Candidate(..)
  , IsValidShrink(..)
  , shrinkExplain
  , limitShrinkSteps
  , shrinkHistory
    -- * Debugging
  , shrinkStep
  ) where

import Data.Bifunctor
import Data.Either
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Stack

import Test.Falsify.Internal.Generator.Definition
import Test.Falsify.Internal.Generator.Truncated
import Test.Falsify.SampleTree (SampleTree(..), Sample(..))

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
shrink = shrinkWithShortcut shortcutMinimal

-- | Generalization of 'shrink'
shrinkWithShortcut :: forall a.
     HasCallStack
  => Shortcut
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
  => Shortcut
  -> (a -> IsValidShrink p n)
  -> Gen a -> SampleTree -> ShrinkExplanation p n
shrinkExplain shortcut p g = \st ->
    case evalSampleTree p g st of
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
        candidates = map (evalSampleTree p g) $ shrinkStep shortcut g st

{-------------------------------------------------------------------------------
  Shrinking candidates
-------------------------------------------------------------------------------}

-- | Candidate during shrink
data Candidate x = Candidate {
      -- | The shrunk 'SampleTree'
      shrunkTree :: SampleTree

      -- | The parts of the shrunk 'SampleTree' the generator looked at
    , truncated :: Truncated

      -- | The result of the generator
    , outcome :: x
    }
  deriving (Functor)

evalSampleTree :: forall a p n.
     (a -> IsValidShrink p n)
  -> Gen a
  -> SampleTree
  -> Either (Candidate p) (Candidate n)
evalSampleTree prop gen shrunkTree =
     uncurry aux . second prop $ runExplain gen shrunkTree
  where
    aux :: Truncated -> IsValidShrink p n -> Either (Candidate p) (Candidate n)
    aux truncated = \case
        ValidShrink   p -> Left  $ mkCandidate p
        InvalidShrink n -> Right $ mkCandidate n
      where
        mkCandidate :: x -> Candidate x
        mkCandidate x = Candidate{shrunkTree, truncated, outcome = x}

{-------------------------------------------------------------------------------
  Shrink step

  This could be considered to be the core of the @falsify@ approach.
-------------------------------------------------------------------------------}

-- | Single step in shrinking
--
-- This is an auxiliary function used in shrinking; users will typically never
-- have to call this function.
shrinkStep :: Shortcut -> Gen a -> SampleTree -> [SampleTree]
shrinkStep shortcut = go
  where
    go :: Gen a -> SampleTree -> [SampleTree]

    -- Tree is already minimal: /cannot/ shrink any further
    go _ Minimal = []

    -- The generator is independent of the tree: /no point/ shrinking
    go (Pure _) _ = []

    -- Actual shrinking only happens for the primitive generator
    -- We cannot shrink if the value is already minimal.
    go (Prim (P f _)) (SampleTree s l r) =
        (\s' -> SampleTree (Shrunk s') l r) <$> f s

    -- For 'Bind' we shrink either the left or the right tree.
    -- As is usual, this introduces a left bias.
    go (Bind x f) (SampleTree s l r) = shortcut . concat $ [
          (\l' -> SampleTree s l' r)  <$> go x             l
        , (\r' -> SampleTree s l  r') <$> go (f $ run x l) r
        ]

    -- The case for 'Select' is similar except that that we shrink the right
    -- subtree /only/ if it used: this is the raison d'être of 'Select'.
    go (Select e f) (SampleTree s l r) = shortcut . concat $ [
          (\l' -> SampleTree s l' r) <$> go e l
        , case run e l of
            Left  _ -> (\r' -> SampleTree s l r') <$> go f r
            Right _ -> []
        ]

{-------------------------------------------------------------------------------
  Shortcuts
-------------------------------------------------------------------------------}

-- | Shortcut shrinking
--
-- A shortcut is a way to shrink "faster" than the normal generator-driven
-- shrinker would shrink. It is given the shrunk sample trees that regular
-- shrinking would compute, and it can modify this list as it sees fit. The
-- identity function is a valid shortcut, but only in the absence of infinite
-- generators (see 'shortcutMinimal').
type Shortcut = [SampleTree] -> [SampleTree]

-- | Introduce the 'Minimal' sample tree /if/ we can minimize at all
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
-- We only introduce 'Minimal' if we can shrink at all; this is an optimization
-- that avoids introducing unnecessary shrink steps.
shortcutMinimal :: Shortcut
shortcutMinimal [] = []
shortcutMinimal ts = Minimal : ts