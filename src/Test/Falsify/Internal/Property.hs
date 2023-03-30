{-# LANGUAGE CPP #-}

-- | Properties
--
-- Intended for unqualified import.
module Test.Falsify.Internal.Property (
    -- * Property
    Property' -- opaque
  , runProperty
    -- * Test results
  , TestResult(..)
  , resultIsValidShrink
    -- * State
  , TestRun(..)
  , Log(..)
  , LogEntry(..)
    -- * Running generators
  , gen
  , genWith
    -- * 'Property' features
  , testFailed
  , info
  , assert
  , discard
    -- * Test shrinking
  , genShrinkPath
  , testShrinkingOfProp
  , testShrinkingOfGen
  , testMinimum
  ) where

import Prelude hiding (log)

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import GHC.Stack

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

import qualified Data.List.NonEmpty as NE

import Test.Falsify.Generator (Gen)
import Test.Falsify.Internal.Generator.Shrinking
import Test.Falsify.Predicate (Predicate, (.$))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

{-------------------------------------------------------------------------------
  Information about a test run
-------------------------------------------------------------------------------}

data TestRun = TestRun {
      runLog :: Log

      -- | Did we generate any values in this test run?
      --
      -- If not, there is no point running the test more than once (with
      -- different seeds), since the test is deterministic.
    , runDeterministic :: Bool
    }
  deriving (Show)

data LogEntry =
    -- | Generated a value
    --
    -- We record the value that was generated as well as /where/ we generated it
    Generated CallStack String

    -- | Some additional information
  | Info String
  deriving (Show)

-- | Log of the events happened during a test run
--
-- The events are recorded in reverse chronological order
newtype Log = Log [LogEntry]
  deriving (Show)

initTestRun :: TestRun
initTestRun = TestRun {
      runLog           = Log []
    , runDeterministic = True
    }

{-------------------------------------------------------------------------------
  Test result
-------------------------------------------------------------------------------}

-- | Test result
data TestResult e a =
    -- | Test was successful
    --
    -- Under normal circumstances @a@ will be @()@.
    TestPassed a

    -- | Test failed
  | TestFailed e

    -- | Test was discarded
    --
    -- This is neither a failure nor a success, but instead is a request to
    -- discard this PRNG seed and try a new one.
  | TestDiscarded
  deriving stock (Show, Functor)

-- | A test result is a valid shrink step if the test still fails
resultIsValidShrink ::
     (TestResult e a, TestRun)
  -> IsValidShrink (e, TestRun) (Maybe a, TestRun)
resultIsValidShrink (TestFailed e  , run) = ValidShrink   (e       , run)
resultIsValidShrink (TestDiscarded , run) = InvalidShrink (Nothing , run)
resultIsValidShrink (TestPassed a  , run) = InvalidShrink (Just a  , run)

{-------------------------------------------------------------------------------
  Monad-transformer version of 'TestResult'
-------------------------------------------------------------------------------}

newtype TestResultT e m a = TestResultT {
      runTestResultT :: m (TestResult e a)
    }
  deriving (Functor)

instance Monad m => Applicative (TestResultT e m) where
  pure x = TestResultT $ pure (TestPassed x)
  (<*>)  = ap

instance Monad m => Monad (TestResultT e m) where
  return  = pure
  x >>= f = TestResultT $ runTestResultT x >>= \case
              TestPassed a  -> runTestResultT (f a)
              TestFailed e  -> pure $ TestFailed e
              TestDiscarded -> pure $ TestDiscarded

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Property
--
-- A 'Property' is a generator that can fail and keeps a track of some
-- information about the test run.
--
-- The @Property@ type synonym for properties that use strings are errors is
-- defined in "Test.Falsify.Property". We do not define it here, so that we
-- cannot by mistake make a function less polymorphic than it should be.
newtype Property' e a = WrapProperty {
      unwrapProperty :: TestResultT e (StateT TestRun Gen) a
    }
  deriving newtype (Functor, Applicative, Monad)

-- | Construct property
--
-- This is a low-level function for internal use only.
mkProperty :: (TestRun -> Gen (TestResult e a, TestRun)) -> Property' e a
mkProperty = WrapProperty . TestResultT . StateT

-- | Run property
runProperty :: Property' e a -> Gen (TestResult e a, TestRun)
runProperty = flip runStateT initTestRun . runTestResultT . unwrapProperty

{-------------------------------------------------------------------------------
  'Property' features
-------------------------------------------------------------------------------}

testFailed :: e -> Property' e a
testFailed err = mkProperty $ \run -> return (TestFailed err, run)

-- | Discard this test
discard :: Property' e a
discard = mkProperty $ \run -> return (TestDiscarded, run)

-- | Log some additional information about the test
--
-- This will be shown in verbose mode.
info :: String -> Property' e ()
info msg =
    mkProperty $ \run@TestRun{runLog = Log log} -> return (
        TestPassed ()
      , run{runLog = Log $ Info msg : log}
      )

-- | Assert predicate
assert :: Predicate '[] -> Property' String ()
assert p =
    case P.eval p of
      Left err -> testFailed err
      Right () -> return ()

instance MonadFail (Property' String) where
  fail = testFailed

{-------------------------------------------------------------------------------
  Running generators
-------------------------------------------------------------------------------}

-- | Internal auxiliary
genWithCallStack :: forall e a.
     CallStack           -- ^ Explicit argument to avoid irrelevant entries
                         -- (users don't care that 'gen' uses 'genWith').
  -> (a -> Maybe String) -- ^ Entry to add to the log (if any)
  -> Gen a -> Property' e a
genWithCallStack stack f g = mkProperty $ \run -> aux run <$> g
  where
    aux :: TestRun -> a -> (TestResult e a, TestRun)
    aux run@TestRun{runLog = Log log} x = (
          TestPassed x
        , run{ runLog = Log $ case f x of
                 Just entry -> Generated stack entry : log
                 Nothing    -> log
             , runDeterministic = False
             }
        )

-- | Generate value and add it to the log
gen :: (HasCallStack, Show a) => Gen a -> Property' e a
gen = genWithCallStack callStack (Just . show)

-- | Generalization of 'gen' that doesn't depend on a 'Show' instance
--
-- No log entry is added if 'Nothing'.
genWith :: HasCallStack => (a -> Maybe String) -> Gen a -> Property' e a
genWith = genWithCallStack callStack

{-------------------------------------------------------------------------------
  Test shrinking
-------------------------------------------------------------------------------}

-- | Append log from another test run to the current test run
--
-- This is an internal function, used when testing shrinking to include the runs
-- from an unshrunk test and a shrunk test.
appendLog :: Log -> Property' e ()
appendLog (Log log') = mkProperty $ \run@TestRun{runLog = Log log} -> return (
      TestPassed ()
    , run{runLog = Log $ log' ++ log}
    )

-- | Construct random path through the property's shrink tree
--
-- A property is normally only shrunk when it /fails/. We do the same here:
-- if the property succeeds, we return an empty list of errors.
--
-- If the given property discards immediately, then this generator will
-- discard also; otherwise, only shrink steps are considered that do not lead
-- to a discard.
genShrinkPath :: Property' e () -> Property' e' [(e, TestRun)]
genShrinkPath prop = do
    st    <- genWith (const Nothing) $ Gen.toShrinkTree (runProperty prop)
    mPath <- genWith (const Nothing) $ Gen.path resultIsValidShrink st
    aux mPath
  where
    aux ::
         Either (Maybe (), TestRun) (NonEmpty (e, TestRun))
      -> Property' e' [(e, TestRun)]
    aux (Left (Just (), _)) = return []
    aux (Left (Nothing, _)) = discard
    aux (Right es)          = return $ toList es

-- | Test shrinking of a property
--
-- See 'genPath' for considerations regarding test failures/discards.
testShrinkingOfProp :: forall e.
     Show e
  => Predicate [e, e] -> Property' e () -> Property' String ()
testShrinkingOfProp p prop = do
    path <- genShrinkPath prop
    case findCounterExample (toList path) of
      Nothing ->
        return ()
      Just (err, logBefore, logAfter) -> do
        info "Before shrinking:"
        appendLog logBefore
        info "After shrinking:"
        appendLog logAfter
        testFailed err
  where
    findCounterExample :: [(e, TestRun)] -> Maybe (String, Log, Log)
    findCounterExample = \case
        []  -> Nothing
        [_] -> Nothing
        ((x, runX) : rest@((y, runY) : _)) ->
          case P.eval $ p .$ ("original", x) .$ ("shrunk", y) of
            Left err -> Just (err, runLog runX, runLog runY)
            Right () -> findCounterExample rest

-- | Test shrinking of a generator
--
-- Since a generator doesn't have a concept of \"failing\", we just /any/
-- shrink step that the generator can make.
testShrinkingOfGen :: Show a => Predicate [a, a] -> Gen a -> Property' String ()
testShrinkingOfGen p g = testShrinkingOfProp p $ gen g >>= testFailed

-- | Test the minimum error thrown by the property
--
-- If the given property passes, we will discard this test (in that case, there
-- is nothing to test); this test is also discarded if the given property
-- discards.
testMinimum :: forall e.
     Show e
  => Predicate '[e]
  -> Property' e ()
  -> Property' String ()
testMinimum p prop = do
    st <- genWith (const Nothing) $ Gen.captureLocalTree
    case Gen.runGen (runProperty prop) st of
      ((TestPassed (), _run), _truncated, _shrunk) ->
        -- The property passed; nothing to test
        discard
      ((TestDiscarded, _run), _truncated, _shrunk) ->
        -- The property needs to be discarded; discard this one, too
        discard
      ((TestFailed e, run), _truncated, shrunk) -> do
        let explanation :: ShrinkExplanation (e, TestRun) (Maybe (), TestRun)
            explanation = shrinkFrom
                            resultIsValidShrink
                            (runProperty prop)
                            ((e, run), shrunk)

            minErr :: e
            minRun :: TestRun
            (minErr, minRun) = NE.last $ shrinkHistory explanation

        case P.eval $ p .$ ("minimum", minErr) of
          Right () -> return ()
          Left err -> do
            appendLog (runLog minRun)
            testFailed err
