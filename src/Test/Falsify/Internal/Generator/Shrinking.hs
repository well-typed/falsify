module Test.Falsify.Internal.Generator.Shrinking (
    -- * Shrinking
    shrink
    -- * With full history
  , ShrinkExplanation(..)
  , ShrinkHistory(..)
  , IsValidShrink(..)
  , shrinkExplain
  , limitShrinkSteps
  , shrinkHistory
  , Shortcut
  , shortcutMinimal
    -- * Debugging
  , shrinkStep
  ) where

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
      initial :: (Truncated, p)

      -- | The full shrink history
    , history :: ShrinkHistory p n
    }

-- | Shrink explanation
data ShrinkHistory p n =
    -- | We successfully executed a single shrink step
    ShrunkTo (Truncated, p) (ShrinkHistory p n)

    -- | We could no shrink any further
    --
    -- We also record all rejected next steps. This is occasionally useful when
    -- trying to figure out why a value didn't shrink any further (what did it
    -- try to shrink to?)
  | ShrinkingDone [(Truncated, n)]

    -- | We stopped shrinking early
    --
    -- This is used when the number of shrink steps is limited.
  | ShrinkingStopped
  deriving (Show)

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
shrinkHistory (ShrinkExplanation (_, unshrunk) shrunk) =
    unshrunk :| go shrunk
  where
    go :: ShrinkHistory p n -> [p]
    go (ShrunkTo (_, x) xs) = x : go xs
    go (ShrinkingDone _)    = []
    go ShrinkingStopped     = []

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
shrink p g = shrinkHistory . shrinkExplain shortcutMinimal p' g
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
    let (trunc, a) = runExplain g st in
    case p a of
      ValidShrink x   -> ShrinkExplanation (trunc, x) $ go st
      InvalidShrink _ -> error "shrink: precondition violated"
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
          ([], rejected) ->
            ShrinkingDone $ map (\c -> (truncated c, outcome c)) rejected
          (c:_, _) ->
            ShrunkTo (truncated c, outcome c) $ go (shrunkTree c)
      where
        candidates :: [Either (Candidate p) (Candidate n)]
        candidates =
            map (eitherCandidate . mkCandidate p g) $
              shrinkStep shortcut g st

{-------------------------------------------------------------------------------
  Shrinking candidates

  This is an internal auxiliary type only; it is not exported.
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

mkCandidate :: forall a x. (a -> x) -> Gen a -> SampleTree -> Candidate x
mkCandidate f g shrunkTree =
    aux $ runExplain g  shrunkTree
  where
    aux :: (Truncated, a) -> Candidate x
    aux (truncated, a) = Candidate{
          shrunkTree
        , truncated
        , outcome = f a
        }

eitherCandidate ::
     Candidate (IsValidShrink p n)
  -> Either (Candidate p) (Candidate n)
eitherCandidate c@Candidate{outcome} =
    case outcome of
      ValidShrink   x -> Left  c{outcome = x}
      InvalidShrink x -> Right c{outcome = x}

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

    -- Finally, for 'Bind' we shrink either the left or the right tree; as is
    -- usual, this introduces a left bias.
    go (Bind x f) (SampleTree s l r) = shortcut . concat $ [
          (\l' -> SampleTree s l' r)  <$> go x             l
        , (\r' -> SampleTree s l  r') <$> go (f $ run x l) r
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