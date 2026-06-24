module Test.Falsify.Internal.Shrinking (
    -- * Shrinking
    shrinkFrom
    -- * With full history
  , ShrinkExplanation(..)
  , ShrinkHistory(..)
  , IsValidShrink(..)
  , isValidShrink
  , shrinkHistory
  , shrinkOutcome
  ) where

import Data.Bifunctor
import Data.Either
import Data.List.NonEmpty (NonEmpty((:|)))

import Test.Falsify.Context (Context(Context))
import Test.Falsify.Internal.Generator
import Test.Falsify.SampleTree (SampleTree(..))

import qualified Test.Falsify.Context as Context

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
    -- We record
    --
    -- * All rejected next steps
    -- * The outcome of the repeated 'finalShrink' step
    --
    -- Recording the rejected next steps is occasionally useful when trying to
    -- figure out why a value didn't shrink any further (what did it try to
    -- shrink to?).
  | ShrinkingDone [n] p

    -- | We stopped shrinking early
    --
    -- This is used when the number of shrink steps is limited.
    -- We record the outcome of the repeated 'finalShrink' step.
  | ShrinkingStopped p
  deriving (Show)

-- | Simplify the shrink explanation to keep only the shrink history
shrinkHistory :: ShrinkExplanation p n -> NonEmpty p
shrinkHistory = \(ShrinkExplanation unshrunk shrunk) ->
    unshrunk :| go shrunk
  where
    go :: ShrinkHistory p n -> [p]
    go (ShrunkTo x xs)       = x : go xs
    go (ShrinkingDone _ns p) = [p]
    go (ShrinkingStopped  p) = [p]

-- | The final shrunk value, as well as all rejected /next/ shrunk steps
--
-- The list of rejected next steps is
--
-- * @Nothing@ if shrinking was terminated early ('limitShrinkSteps')
-- * @Just []@ if the final value truly is minimal (typically, it is only
--   minimal wrt to a particular properly, but not the minimal value that a
--   generator can produce).
shrinkOutcome :: forall p n. ShrinkExplanation p n -> (p, Maybe [n])
shrinkOutcome = \ShrinkExplanation{history} ->
    go history
  where
    go :: ShrinkHistory p n -> (p, Maybe [n])
    go (ShrunkTo _p h)      = go h
    go (ShrinkingDone ns p) = (p, Just ns)
    go (ShrinkingStopped p) = (p, Nothing)

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
      ShrunkTo         p h -> ShrunkTo                 (f p) (bimap f g h)
      ShrinkingDone ns p   -> ShrinkingDone (map g ns) (f p)
      ShrinkingStopped p   -> ShrinkingStopped         (f p)

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

-- | Find smallest value that the generator can produce and still satisfies
-- the predicate.
--
-- Returns the full shrink history.
--
-- To avoid boolean blindness, we use different types for values that satisfy
-- the property and values that do not.
--
-- This is lazy in the shrink history.
shrinkFrom :: forall p n.
     Context.Static
     -- ^ Static context
     --
     -- Passed as-is to the property as part of the 'Context'.
     -- Used to extract shrinking parameters.
  -> Context.Iteration
     -- ^ Iteration context
     --
     -- Passsed as-is to the property as part of the 'Context'.
     -- Not used otherwise.
  -> (Context -> Gen (IsValidShrink p n))
     -- ^ The property we're shrinking
  -> SampleTree
     -- ^ The sample tree we started with
  -> (p, [SampleTree])
     -- ^ The initial result of the generator
  -> ShrinkExplanation p n
shrinkFrom static iteration prop = \st (p, shrunk) ->
    ShrinkExplanation p $ go 0 st shrunk
  where
    go :: Word -> SampleTree -> [SampleTree] -> ShrinkHistory p n
    go i = \st shrunk ->
        -- NOTE: 'partitionEithers' is lazy enough:
        --
        -- > head . fst $ partitionEithers [Left True, undefined] == True
        let candidates :: [(SampleTree, p, [SampleTree])]
            rejected   :: [n]
            (candidates, rejected) = partitionEithers $ map consider shrunk

         in case candidates of
              [] ->
                ShrinkingDone rejected $ runFinal i st
              (st', p, shrunk'):_ | canContinue ->
                ShrunkTo p $ go (succ i) st' shrunk'
              _otherwise ->
                ShrinkingStopped $ runFinal i st
      where
        ctx :: Context
        ctx = Context static iteration $ Context.Shrinking i

        canContinue :: Bool
        canContinue =
             maybe
               True
               (\limit -> i < limit)
               (Context.maxShrinks static)

        consider :: SampleTree -> Either (SampleTree, p, [SampleTree]) n
        consider st =
            case isValid of
              ValidShrink p   -> Left (st, p, shrunk')
              InvalidShrink n -> Right n
          where
            isValid :: IsValidShrink p n
            shrunk' :: [SampleTree]
            (isValid, shrunk') = runGen (prop ctx) st

    -- Run the property one final time
    --
    -- Precondition: must be run on a sample tree for which we know the
    -- property fails.
    runFinal :: Word -> SampleTree -> p
    runFinal i st =
        case fst $ runGen (prop ctx) st of
          ValidShrink   p -> p
          InvalidShrink _ -> error "runFinal: precondition violated"
      where
        ctx :: Context
        ctx = Context static iteration $ Context.Final i
