{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
module Test.Falsify.Internal.Generator.Shrinking (
    -- * Shrinking
    shrinkFrom
  , shrinkFromIO
    -- * With full history
  , ShrinkExplanation(..)
  , ShrinkHistory(..)
  , IsValidShrink(..)
  , limitShrinkSteps
  , shrinkHistory
  , shrinkOutcome
  ) where

import Data.Bifunctor
import Data.Either
import Data.List.NonEmpty (NonEmpty((:|)))

import Test.Falsify.Internal.Generator.Definition
import Test.Falsify.Internal.SampleTree (SampleTree(..))
import {-# SOURCE #-} Test.Falsify.Internal.Property (TestResult, TestRun)

{-------------------------------------------------------------------------------
  Explanation
-------------------------------------------------------------------------------}

-- | Shrink explanation
--
-- @p@ is the type of \"positive\" elements that satisfied the predicate (i.e.,
-- valid shrinks), and @n@ is the type of \"negative\" which didn't.
data ShrinkExplanation p n = ShrinkExplanation {
      -- | The value we started, before shrinking
      initial :: p

      -- | The full shrink history
    , history :: ShrinkHistory p n
    }
  deriving (Show)

-- | Shrink explanation
data ShrinkHistory p n =
    -- | We successfully executed a single shrink step
    ShrunkTo p (ShrinkHistory p n)

    -- | We could no shrink any further
    --
    -- We also record all rejected next steps. This is occasionally useful when
    -- trying to figure out why a value didn't shrink any further (what did it
    -- try to shrink to?)
  | ShrinkingDone [n]

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
shrinkHistory = \(ShrinkExplanation unshrunk shrunk) ->
    unshrunk :| go shrunk
  where
    go :: ShrinkHistory p n -> [p]
    go (ShrunkTo x xs)   = x : go xs
    go (ShrinkingDone _) = []
    go ShrinkingStopped  = []

-- | The final shrunk value, as well as all rejected /next/ shrunk steps
--
-- The list of rejected next steps is
--
-- * @Nothing@ if shrinking was terminated early ('limitShrinkSteps')
-- * @Just []@ if the final value truly is minimal (typically, it is only
--   minimal wrt to a particular properly, but not the minimal value that a
--   generator can produce).
shrinkOutcome :: forall p n. ShrinkExplanation p n -> (p, Maybe [n])
shrinkOutcome = \ShrinkExplanation{initial, history} ->
    go initial history
  where
    go :: p -> ShrinkHistory p n -> (p, Maybe [n])
    go _ (ShrunkTo p h)     = go p h
    go p (ShrinkingDone ns) = (p, Just ns)
    go p  ShrinkingStopped  = (p, Nothing)

{-------------------------------------------------------------------------------
  Mapping
-------------------------------------------------------------------------------}

instance Functor (ShrinkExplanation p) where
  fmap = second

instance Functor (ShrinkHistory p) where
  fmap = second

instance Bifunctor ShrinkExplanation where
  bimap f g ShrinkExplanation{initial, history} = ShrinkExplanation{
        initial = f initial
      , history = bimap f g history
      }

instance Bifunctor ShrinkHistory where
  bimap f g = \case
      ShrunkTo truncated history ->
        ShrunkTo (f truncated) (bimap f g history)
      ShrinkingDone rejected ->
        ShrinkingDone (map g rejected)
      ShrinkingStopped ->
        ShrinkingStopped

{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

-- | Does a given shrunk value represent a valid shrink step?
data IsValidShrink p n =
    ValidShrink p
  | InvalidShrink n
  deriving stock (Show)

-- | Find smallest value that the generator can produce and still satisfies
-- the predicate.
--
-- Returns the full shrink history.
--
-- To avoid boolean blindness, we use different types for values that satisfy
-- the property and values that do not.
--
-- This is lazy in the shrink history; see 'limitShrinkSteps' to limit the
-- number of shrinking steps.
shrinkFrom :: forall a p n.
     (a -> IsValidShrink p n)
  -> Gen a
  -> (p, [SampleTree]) -- ^ Initial result of the generator
  -> ShrinkExplanation p n
shrinkFrom prop gen = \(p, shrunk) ->
    ShrinkExplanation p $ go shrunk
  where
    go :: [SampleTree] -> ShrinkHistory p n
    go shrunk =
        -- Shrinking is a greedy algorithm: we go with the first candidate that
        -- works, and discard the others.
        --
        -- NOTE: 'partitionEithers' is lazy enough:
        --
        -- > head . fst $ partitionEithers [Left True, undefined] == True
        case partitionEithers candidates of
          ([], rejected)      -> ShrinkingDone rejected
          ((p, shrunk'):_, _) -> ShrunkTo p $ go shrunk'
      where
        candidates :: [Either (p, [SampleTree]) n]
        candidates = map consider $ map (runGen gen) shrunk

    consider :: (a, [SampleTree]) -> Either (p, [SampleTree]) n
    consider (a, shrunk) =
        case prop a of
          ValidShrink p   -> Left (p, shrunk)
          InvalidShrink n -> Right n

shrinkFromIO :: forall a p n.
     ((TestResult String (IO a), TestRun) -> IO (IsValidShrink p n))
  -> Gen (TestResult String (IO a), TestRun)
  -> (p, [SampleTree]) -- ^ Initial result of the generator
  -> IO (ShrinkExplanation p n)
shrinkFromIO prop gen = \(p, shrunk) ->
    ShrinkExplanation p <$> go shrunk
  where
    go :: [SampleTree] -> IO (ShrinkHistory p n)
    go shrunk = do
        -- Shrinking is a greedy algorithm: we go with the first candidate that
        -- works, and discard the others.
        --
        -- NOTE: 'partitionEithers' is lazy enough:
        --
        -- > head . fst $ partitionEithers [Left True, undefined] == True
        partitionEithers <$> candidates >>= \case
          ([], rejected)      -> pure $ ShrinkingDone rejected
          ((p, shrunk'):_, _) -> ShrunkTo p <$> go shrunk'
      where
        candidates :: IO [Either (p, [SampleTree]) n]
        candidates =
            traverse (consider . runGen gen) shrunk

    consider :: ((TestResult String (IO a), TestRun), [SampleTree]) -> IO (Either (p, [SampleTree]) n)
    consider (a, shrunk) =
        prop a >>= \case
          ValidShrink p   -> pure $ Left (p, shrunk)
          InvalidShrink n -> pure $ Right n
