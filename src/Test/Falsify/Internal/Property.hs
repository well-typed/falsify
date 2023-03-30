{-# LANGUAGE CPP #-}

-- | Properties
--
-- Intended for unqualified import.
module Test.Falsify.Internal.Property (
    -- * Property
    Property -- opaque
  , TestResult(..)
  , runProperty
    -- * State
  , TestRun(..)
  , Log(..)
  , LogEntry(..)
    -- * Running generators
  , gen
  , genWith
    -- * Other 'Property' features
  , info
  , assert
  , discard
  , discardIf
    -- * Test shrinking
  , genShrinkPath
  , testShrinking
  ) where

import Prelude hiding (log)

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (toList)
import GHC.Stack

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

import Test.Falsify.Generator (Gen)
import Test.Falsify.Predicate (Predicate, (.$))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as Predicate
import qualified Test.Falsify.Predicate as P
import Data.List.NonEmpty (NonEmpty)

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
data TestResult a =
    -- | Test was successful
    --
    -- Under normal circumstances @a@ will be @()@.
    TestPassed a

    -- | Test failed
  | TestFailed String

    -- | Test was discarded
    --
    -- This is neither a failure nor a success, but instead is a request to
    -- discard this PRNG seed and try a new one.
  | TestDiscarded
  deriving (Functor)

instance Applicative TestResult where
  pure  = TestPassed
  (<*>) = ap

instance Monad TestResult where
  return = pure
  TestPassed x  >>= f = f x
  TestFailed e  >>= _ = TestFailed e
  TestDiscarded >>= _ = TestDiscarded

{-------------------------------------------------------------------------------
  Monad-transformer version of 'TestResult'
-------------------------------------------------------------------------------}

newtype TestResultT m a = TestResultT {
      runTestResultT :: m (TestResult a)
    }
  deriving (Functor)

instance Monad m => Applicative (TestResultT m) where
  pure x = TestResultT $ pure (TestPassed x)
  (<*>)  = ap

instance Monad m => Monad (TestResultT m) where
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
newtype Property a = WrapProperty {
      unwrapProperty :: TestResultT (StateT TestRun Gen) a
    }
  deriving newtype (Functor, Applicative, Monad)

-- | Construct property
--
-- This is a low-level function for internal use only.
mkProperty :: (TestRun -> Gen (TestResult a, TestRun)) -> Property a
mkProperty = WrapProperty . TestResultT . StateT

-- | Run property
runProperty :: Property a -> Gen (TestResult a, TestRun)
runProperty = flip runStateT initTestRun . runTestResultT . unwrapProperty

{-------------------------------------------------------------------------------
  'Property' features
-------------------------------------------------------------------------------}

instance MonadFail Property where
  fail err = mkProperty $ \run -> return (TestFailed err, run)

-- | Discard this test
discard :: Property a
discard = mkProperty $ \run -> return (TestDiscarded, run)

-- | Conditionally discard the test
--
-- This is just a convenience function around 'discard'
discardIf :: Bool -> Property ()
discardIf False = return ()
discardIf True  = discard

-- | Log some additional information about the test
--
-- This will be shown in verbose mode.
info :: String -> Property ()
info msg =
    mkProperty $ \run@TestRun{runLog = Log log} -> return (
        TestPassed ()
      , run{runLog = Log $ Info msg : log}
      )

-- | Assert predicate
assert :: Predicate '[] -> Property ()
assert p =
    case Predicate.eval p of
      Left err -> fail err
      Right () -> return ()

{-------------------------------------------------------------------------------
  Running generators
-------------------------------------------------------------------------------}

-- | Generate value and add it to the log
gen :: (HasCallStack, Show a) => Gen a -> Property a
gen = genWithCallStack callStack (Just . show)

-- | Generalization of 'gen' that doesn't depend on a 'Show' instance
--
-- No log entry is added if 'Nothing'.
genWith :: HasCallStack => (a -> Maybe String) -> Gen a -> Property a
genWith = genWithCallStack callStack

-- | Internal auxiliary
genWithCallStack :: forall a.
     CallStack           -- ^ Explicit argument to avoid irrelevant entries
                         -- (users don't care that 'gen' uses 'genWith').
  -> (a -> Maybe String) -- ^ Entry to add to the log (if any)
  -> Gen a -> Property a
genWithCallStack stack f g = mkProperty $ \run -> aux run <$> g
  where
    aux :: TestRun -> a -> (TestResult a, TestRun)
    aux run@TestRun{runLog = Log log} x = (
          TestPassed x
        , run{ runLog = Log $ case f x of
                 Just entry -> Generated stack entry : log
                 Nothing    -> log
             , runDeterministic = False
             }
        )

{-------------------------------------------------------------------------------
  Test shrinking
-------------------------------------------------------------------------------}

-- | Append log from another test run to the current test run
--
-- This is an internal function, used when testing shrinking to include the runs
-- from an unshrunk test and a shrunk test.
appendLog :: Log -> Property ()
appendLog (Log log') = mkProperty $ \run@TestRun{runLog = Log log} -> return (
      TestPassed ()
    , run{runLog = Log $ log' ++ log}
    )

-- | Construct random path through the property's shrink tree
--
-- If the given 'Property' fails immediately, this generator fails also;
-- similarly, if the given property is immediately discarded, this generator is
-- also discarded. Otherwise, only shrink steps are considered that do not lead
-- to a test failure or a test discard.
--
-- Note that this is opposite to how normal shrinking words: normal shrinking
-- looks for the smallest test that /fails/; here we look for the smallest test
-- that /succeeds/. The reason for this inversion is that failing tests have
-- no interesting results, so there is nothing to verify. TODO: We could change
-- that?
genShrinkPath :: Property a -> Property (NonEmpty (a, TestRun))
genShrinkPath prop = do
    st    <- genWith (const Nothing) $ Gen.toShrinkTree (runProperty prop)
    mPath <- genWith (const Nothing) $ Gen.path validShrink st
    case mPath of
      Left Nothing  -> discard
      Left (Just e) -> fail e
      Right path    -> return path
  where
    validShrink :: (TestResult a, TestRun) -> Either (Maybe String) (a, TestRun)
    validShrink (TestPassed a, run) = Right (a, run)
    validShrink (TestFailed e, _)   = Left (Just e)
    validShrink (TestDiscarded, _)  = Left Nothing

-- | Test shrinking
--
-- See 'genPath' for considerations regarding test failures/discards.
testShrinking :: forall a. Show a => Predicate [a, a] -> Property a -> Property ()
testShrinking p prop = do
    path <- genShrinkPath prop
    case findCounterExample (toList path) of
      Nothing ->
        return ()
      Just (err, logBefore, logAfter) -> do
        info "Before shrinking:"
        appendLog logBefore
        info "After shrinking:"
        appendLog logAfter
        fail err
  where
    findCounterExample :: [(a, TestRun)] -> Maybe (String, Log, Log)
    findCounterExample = \case
        []  -> Nothing
        [_] -> Nothing
        ((x, runX) : rest@((y, runY) : _)) ->
          case P.eval $ p .$ ("original", x) .$ ("shrunk", y) of
            Left err -> Just (err, runLog runX, runLog runY)
            Right () -> findCounterExample rest
