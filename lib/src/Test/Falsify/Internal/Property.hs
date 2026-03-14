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
  , resultIsValidShrinkIO
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
  , label
  , collect
    -- * Testing shrinking
  , testShrinking
  , testMinimum
    -- * Testing generators
  , testGen
  , testGen'
  , testShrinkingOfGen
  ) where

import Prelude hiding (log)

import Control.Monad
import Control.Monad.State
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import GHC.Stack

import qualified Data.Map as Map
import qualified Data.Set as Set

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

import Test.Falsify.Generator (Gen)
import Test.Falsify.Internal.Generator.Shrinking
import Test.Falsify.Predicate (Predicate, (.$))

import qualified Test.Falsify.Generator          as Gen
import qualified Test.Falsify.Internal.Generator as Gen
import qualified Test.Falsify.Predicate          as P
import Data.Functor ((<&>))
import Control.Exception (AsyncException, try, Exception (fromException, displayException), throwIO)

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

      -- | Labels
    , runLabels :: Map String (Set String)
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
    , runLabels        = Map.empty
    }

-- | Append log from another test run to the current test run
--
-- This is an internal function, used when testing shrinking to include the runs
-- from an unshrunk test and a shrunk test.
appendLog :: Log -> Property' e ()
appendLog (Log log') = mkProperty $ \run@TestRun{runLog = Log log} -> return (
      TestPassed ()
    , run{runLog = Log $ log' ++ log}
    )

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
resultIsValidShrink (result, run) =
    case result of
      TestFailed e  -> ValidShrink   (e       , run)
      TestDiscarded -> InvalidShrink (Nothing , run)
      TestPassed a  -> InvalidShrink (Just a  , run)

resultIsValidShrinkIO ::
     (TestResult String (IO a), TestRun)
  -> IO (IsValidShrink (String, TestRun) (Maybe a, TestRun))
resultIsValidShrinkIO (result, run) =
    case result of
      TestFailed e     -> pure $ ValidShrink   (e       , run)
      TestDiscarded    -> pure $ InvalidShrink (Nothing , run)
      TestPassed runA  -> do
        try runA >>= \case
            Right a -> pure $ InvalidShrink (Just a, run)
            Left exc ->
                case fromException @AsyncException exc of
                    Just _ ->
                        throwIO exc -- Let async exceptions pass
                    Nothing ->
                        -- failed again
                        pure $ ValidShrink (displayException exc, run)

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

  The @Property@ type synonym for properties that use strings are errors is
  defined in "Test.Falsify.Property". We do not define it here, so that we
  cannot by mistake make a function less polymorphic than it should be.
-------------------------------------------------------------------------------}

-- | Property
--
-- A 'Property' is a generator that can fail and keeps a track of some
-- information about the test run.
--
-- In most cases, you will probably want to use 'Test.Falsify.Property.Property'
-- instead, which fixes @e@ at 'String'.
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

-- | Test failure
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

-- | Fail the test if the predicate does not hold
assert :: Predicate '[] -> Property' String ()
assert p =
    case P.eval p of
      Left err -> testFailed err
      Right () -> return ()

-- | Variation on 'collect' that does not rely on 'Show'
--
-- See 'collect' for detailed discussion.
label :: String -> [String] -> Property' e ()
label lbl vals =
    mkProperty $ \run@TestRun{runLabels} -> return (
        TestPassed ()
      , run{runLabels = Map.alter addValues lbl runLabels}
      )
  where
    addValues :: Maybe (Set String) -> Maybe (Set String)
    addValues = Just . Set.union (Set.fromList vals) . fromMaybe Set.empty

-- | Label this test
--
-- See also 'label', which does not rely on 'Show'.
--
-- === Motivation
--
-- Labelling is instrumental in understanding the distribution of test data. For
-- example, consider testing a binary tree type, and we want to test some
-- properties of an @insert@ operation (example from "How to specify it!" by
-- John Hughes):
--
-- > prop_insert_insert :: Property ()
-- > prop_insert_insert = do
-- >   tree     <- gen $ ..
-- >   (k1, v1) <- gen $ ..
-- >   (k2, v2) <- gen $ ..
-- >   assert $ .. (insert k1 v1 $ insert k2 v2 $ tree) ..
--
-- We might want to know in what percentage of tests @k1 == k2@:
--
-- > collect "sameKey" [k1 == k2]
--
-- When we do, @falsify@ will report in which percentage of tests the key
-- are the same, and in which percentage of tests they are not.
--
-- === Labels with multiple values
--
-- In general, a particular label can have multiple values in any given test
-- run. Given a test of @n@ test runs, for each value @v@ reported, @falsify@
-- will report what percentage of the @n@ runs are labelled with @v@. That means
-- that these percentages /may/ not add up to 100%; indeed, if we had
--
-- > collect "sameKey" [True]
-- > ..
-- > collect "sameKey" [False]
--
-- or, equivalently,
--
-- > collect "sameKey" [True, False]
--
-- then /every/ test would have been reported as labelled with @True@ (100%@)
-- /as well as/ with @False@ (also 100%). Of course, if we do (like above)
--
-- > collect "sameKey" [k1 == k2]
--
-- each test will be labelled with /either/ @True@ /or/ @False@, and the
-- percentages /will/ add up to 100%.
--
-- === Difference from QuickCheck
--
-- Since you can call @collect@ anywhere in a property, it is natural that the
-- same label can have /multiple/ values in any given test run. In this regard,
-- @collect@ is closer to QuickCheck's @tabulate@. However, the statistics of
-- @tabulate@ can be difficult to interpret; QuickCheck reports the frequency of
-- a value as a percentage of the /total number of values collected/; the
-- frequency reported by @falsify@ here is always in terms of number of test
-- runs, like @collect@ does in QuickCheck. We therefore opted to use the name
-- @collect@ rather than @tabulate@.
collect :: Show a => String -> [a] -> Property' e ()
collect l = label l . map show

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
  Internal auxiliary: testing shrinking
-------------------------------------------------------------------------------}

-- | Construct random path through the property's shrink tree
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

{-------------------------------------------------------------------------------
  Test shrinking
-------------------------------------------------------------------------------}

-- | Test shrinking of a property
--
-- A property is normally only shrunk when it /fails/. We do the same here:
-- if the property succeeds, we discard the test and try again.
--
-- If the given property itself discards immediately, then this generator will
-- discard also; otherwise, only shrink steps are considered that do not lead
-- to a discard.
testShrinking :: forall e.
     Show e
  => Predicate [e, e] -> Property' e () -> Property' String ()
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

-- | Test the minimum error thrown by the property
--
-- If the given property passes, we will discard this test (in that case, there
-- is nothing to test); this test is also discarded if the given property
-- discards.
--
-- NOTE: When testing a particular generator, you might still want to test with
-- some particular property in mind. Otherwise, the minimum value will always
-- simply be the value that the generator produces when given the @Minimal@
-- sample tree.
testMinimum :: forall e.
     Show e
  => Predicate '[e]
  -> Property' e ()
  -> Property' String ()
testMinimum p prop = do
    st <- genWith (const Nothing) $ Gen.captureLocalTree
    case Gen.runGen (runProperty prop) st of
      ((TestPassed (), _run), _shrunk) ->
        -- The property passed; nothing to test
        discard
      ((TestDiscarded, _run), _shrunk) ->
        -- The property needs to be discarded; discard this one, too
        discard
      ((TestFailed initErr, initRun), shrunk) -> do
        let explanation :: ShrinkExplanation (e, TestRun) (Maybe (), TestRun)
            explanation = shrinkFrom
                            resultIsValidShrink
                            (runProperty prop)
                            ((initErr, initRun), shrunk)

            minErr    :: e
            minRun    :: TestRun
            mRejected :: Maybe [(Maybe (), TestRun)]
            ((minErr, minRun), mRejected) = shrinkOutcome explanation

            rejected :: [TestRun]
            rejected  = maybe [] (map snd) mRejected

        case P.eval $ p .$ ("minimum", minErr) of
          Right () -> do
            -- For a successful test, we add the full shrink history as info
            -- This means that users can use verbose mode to see precisely
            -- how the minimum value is reached, if they wish.
            info "Shrink history:"
            forM_ (shrinkHistory explanation) $ \(e, _run) ->
              info $ show e
          Left err -> do
            appendLog (runLog minRun)
            unless (null rejected) $ do
              info "\nLogs for rejected potential next shrinks:"
              forM_ (zip [0 :: Word ..] rejected) $ \(i, rej) -> do
                info $ "\n** Rejected run " ++ show i
                appendLog $ runLog rej
            testFailed err

{-------------------------------------------------------------------------------
  Testing generators
-------------------------------------------------------------------------------}

-- | Test output of the generator
testGen :: forall a. Show a => Predicate '[a] -> Gen a -> Property' String ()
testGen p = testGen' $ \a -> P.eval $ p .$ ("generated", a)

-- | Generalization of 'testGen'
testGen' :: forall e a b. (a -> Either e b) -> Gen a -> Property' e b
testGen' p g = WrapProperty $ TestResultT $ StateT $ \run ->
    -- We do not use bind here to avoid introducing new shrinking shortcuts
    aux run <$> g
  where
    aux :: TestRun -> a -> (TestResult e b, TestRun)
    aux run a = (
          case p a of
            Left  e -> TestFailed e
            Right b -> TestPassed b
        , run{runDeterministic = False}
        )

-- | Test shrinking of a generator
--
-- We check /any/ shrink step that the generator can make (independent of any
-- property).
testShrinkingOfGen :: Show a => Predicate [a, a] -> Gen a -> Property' String ()
testShrinkingOfGen p = testShrinking p . testGen' Left

