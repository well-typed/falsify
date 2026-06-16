module Test.Falsify.Internal.Shrinking (
    -- * Shrinking
    shrinkFrom
    -- * With full history
  , ShrinkExplanation(..)
  , ShrinkHistory(..)
  , IsValidShrink(..)
  , isValidShrink
  , finalShrink
  , shrinkHistory
  , shrinkOutcome
  ) where

import Data.Bifunctor
import Data.Either
import Data.List.NonEmpty (NonEmpty((:|)))

import Test.Falsify.Internal.Context (Context)
import Test.Falsify.Internal.Generator
import Test.Falsify.SampleTree (SampleTree(..))

import qualified Test.Falsify.Internal.Context as Context

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

isValidShrink :: IsValidShrink p n -> Either n p
isValidShrink (ValidShrink p)   = Right p
isValidShrink (InvalidShrink n) = Left n

-- | Rerun the final shrink with the context flag 'Test.Falsify.Context.finalShrink'
-- asserted
finalShrink :: forall a p n.
     (a -> IsValidShrink p n)
  -> (Context -> Gen a)
  -> Context
  -> SampleTree
  -> p
finalShrink prop gen ctx st =
    case prop a of
      ValidShrink p -> p
      _             -> error finalErrorMsg
  where
    a =
      fst $
        runGen
          (gen ctx{Context.finalShrink = True})
          st

    finalErrorMsg :: String
    finalErrorMsg = "Inconsistent test: failing test did not fail with finalShrink True"

-- | Find smallest value that the generator can produce and still satisfies
-- the predicate.
--
-- Returns the full shrink history.
--
-- To avoid boolean blindness, we use different types for values that satisfy
-- the property and values that do not.
--
-- This is lazy in the shrink history.
shrinkFrom :: forall a p n.
     Maybe Word -- ^ Limit amount of shrinking steps
  -> (a -> IsValidShrink p n)
  -> (Context -> Gen a)
  -> Context
  -> (p, [SampleTree]) -- ^ Initial result of the generator
  -> ShrinkExplanation p n
shrinkFrom limit prop gen ctx = \(p, shrunk) ->
    ShrinkExplanation p $ go 1 shrunk
  where
    go i shrunk =
        -- Shrinking is a greedy algorithm: we go with the first candidate that
        -- works, and discard the others.
        if | ([], rejected) <- candidates
           -> ShrinkingDone rejected
           | Just li <- limit
           , i > li
           -> ShrinkingStopped
           | ((st, p, shrunk'):_, _) <- candidates
           -> let next :: ShrinkHistory p n
                  next = go (succ i) shrunk'
              in case next of
                   ShrunkTo _ _ -> ShrunkTo p next
                   _            -> ShrunkTo (finalShrink prop gen ctx' st) next
      where
        -- NOTE: 'partitionEithers' is lazy enough:
        --
        -- > head . fst $ partitionEithers [Left True, undefined] == True
        candidates :: ([(SampleTree, p, [SampleTree])], [n])
        candidates = partitionEithers $ map consider shrunk

        consider :: SampleTree -> Either (SampleTree, p, [SampleTree]) n
        consider st =
            case prop a of
              ValidShrink p   -> Left (st, p, shrunk')
              InvalidShrink n -> Right n
          where
            (a, shrunk') = runGen (gen ctx') st

        ctx' :: Context
        ctx' = ctx{Context.thisShrink = Just i}
