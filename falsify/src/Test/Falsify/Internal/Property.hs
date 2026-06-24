{-# LANGUAGE OverloadedStrings #-}

-- | Properties
--
-- Intended for unqualified import.
module Test.Falsify.Internal.Property (
    -- * Property
    Property' -- opaque
  , Property
  , runProperty
    -- * Test results
  , TestResult(..)
  , Counterexample(..)
  , resultIsValidShrink
    -- * State
  , TestRun(..)
  , Log(..)
  , LogEntry(..)
    -- * Running generators
  , gen
  , genWith
    -- * 'Property'' features
  , testFailed
  , info
  , assert
  , discard
  , label
  , collect
  , getContext
    -- * Testing shrinking
  , testShrinking
  , testShrinkingForIteration
  , testMinimum
  , testMinimumForIteration
    -- * Testing generators
  , testGen
  , testGen'
  , testShrinkingOfGen
  , testShrinkingOfGenForIteration
  ) where

import Prelude hiding (log)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import GHC.Stack

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Falsify.Context (Context(Context))
import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Shrinking
import Test.Falsify.Predicate (Predicate, (.$))

import qualified Test.Falsify.Context   as Context
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

{-------------------------------------------------------------------------------
  Information about a test run
-------------------------------------------------------------------------------}

data TestRun = TestRun {
      -- | Any 'info' messages generated during the run
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
appendLog (Log log') = mkProperty $ \_ctx run@TestRun{runLog = Log log} -> return (
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

data Counterexample e = Counterexample{
      counterexampleContext :: Context.Execution
    , counterexampleError   :: e
    , counterexampleRun     :: TestRun
    }
  deriving (Show)

resultIsValidShrink ::
     Context.Execution
  -> (TestResult e a, TestRun)
  -> IsValidShrink (Counterexample e) (Maybe a, TestRun)
resultIsValidShrink ctxt (result, run) =
    case result of
      TestFailed e  -> ValidShrink $ Counterexample ctxt e run
      TestDiscarded -> InvalidShrink (Nothing, run)
      TestPassed a  -> InvalidShrink (Just a , run)

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
-- A 'Property'' is a generator that can fail and keeps a track of some
-- information about the test run.
--
-- In most cases, you will probably want to use t'Test.Falsify.Property.Property'
-- instead, which fixes @e@ at 'String'.
newtype Property' e a = WrapProperty {
      unwrapProperty :: TestResultT e (ReaderT Context (StateT TestRun Gen)) a
    }
  deriving newtype (Functor, Applicative, Monad)

-- | Property that uses strings as errors
--
-- Most of @falsify@'s internal functions work with 'Property'', but most
-- user-facing functions use 'Property' instead.
type Property = Property' String

-- | Construct property
--
-- This is a low-level function for internal use only.
mkProperty :: (Context -> TestRun -> Gen (TestResult e a, TestRun)) -> Property' e a
mkProperty f = WrapProperty $ TestResultT $ ReaderT $ \ctx -> StateT (f ctx)

-- | Run property
runProperty :: Property' e a -> Context -> Gen (TestResult e a, TestRun)
runProperty p ctx =
    flip runStateT initTestRun $ flip runReaderT ctx $ runTestResultT $ unwrapProperty p

{-------------------------------------------------------------------------------
  'Property' features
-------------------------------------------------------------------------------}

-- | Test failure
testFailed :: e -> Property' e a
testFailed err = mkProperty $ \_ctx run -> return (TestFailed err, run)

-- | Discard this test
discard :: Property' e a
discard = mkProperty $ \_ctx run -> return (TestDiscarded, run)

-- | Log some additional information about the test
--
-- This will be shown in verbose mode.
info :: String -> Property' e ()
info msg =
    mkProperty $ \_ctx run@TestRun{runLog = Log log} -> return (
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
    mkProperty $ \_ctx run@TestRun{runLabels} -> return (
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


-- | Get the context for the current test run
getContext :: Property' e Context
getContext = mkProperty $ \ctx run -> return (TestPassed ctx, run)

{-------------------------------------------------------------------------------
  Running generators
-------------------------------------------------------------------------------}

-- | Internal auxiliary
genWithCallStack :: forall e a.
     CallStack           -- ^ Explicit argument to avoid irrelevant entries
                         -- (users don't care that 'gen' uses 'genWith').
  -> (a -> Maybe String) -- ^ Entry to add to the log (if any)
  -> Gen a -> Property' e a
genWithCallStack stack f g = mkProperty $ \_ctx run -> aux run <$> g
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
--
-- The repeated 'finalShrink' step is /not/ included.
genShrinkPath ::
     Context.Iteration -- ^ See 'testShrinking' for detailed discussion
  -> Property' e ()
  -> Property' e' [Counterexample e]
genShrinkPath iteration prop = do
    static <- Context.static <$> getContext

    let ctx :: Context.Execution -> Context
        ctx = Context static iteration

    st    <- genWith (const Nothing) $
               Gen.toShrinkTreeWithContext False (runProperty prop . ctx)
    mPath <- genWith (const Nothing) $
               Gen.path
                 ( \(exe, (result, run)) ->
                      isValidShrink $ resultIsValidShrink exe (result, run)
                 )
                 st
    aux mPath
  where
    aux ::
         Either (Maybe (), TestRun) (NonEmpty (Counterexample e))
      -> Property' e' [Counterexample e]
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
--
-- See also 'testShrinkingForIteration'.
testShrinking :: forall e.
     Show e
  => Predicate [e, e]
  -> Property' e () -> Property' String ()
testShrinking = testShrinkingForIteration $ Context.Iteration{
      thisTest = error $ concat [
          "thisTest is undefined. "
        , "Use testShrinkingForIteration "
        , "instead of testShrinking."
        ]
    }

-- | Generalization of 'testShrinking' for an arbitrary 'Iteration'
--
-- Some properties may behave quite differently given a different iteration
-- context, in which case it is important to be explicit about this.
--
-- The /shrinking/ context is constructed so that it accurately reflects
-- the path: 'Nothing' for the root of the tree, and then 'Just' the shrink
-- step as we follow edges downwards. There is /no/ repeated 'finalShrink'
-- step: 'genShrinkPath' is typically used to verify that successive shrink
-- steps result in values that are closer to the generator's origin, which
-- is trivially violated by that repeated final step.
--
-- The /static/ context is inherited from the parent property.
testShrinkingForIteration :: forall e.
     Show e
  => Context.Iteration
  -> Predicate [e, e] -> Property' e () -> Property' String ()
testShrinkingForIteration iteration p prop = do
    path <- genShrinkPath iteration prop
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
    findCounterExample :: [Counterexample e] -> Maybe (String, Log, Log)
    findCounterExample = \case
        []  -> Nothing
        [_] -> Nothing
        (x : rest@(y : _)) ->
          case P.eval $
                 p .$ ("original" , counterexampleError x)
                   .$ ("shrunk"   , counterexampleError y) of
            Right () -> findCounterExample rest
            Left err -> Just (
                err
              , runLog (counterexampleRun x)
              , runLog (counterexampleRun y)
              )

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
--
-- See also 'testMinimumForIteration'.
testMinimum :: Show e => Predicate '[e] -> Property' e () -> Property' String ()
testMinimum = testMinimumForIteration $ Context.Iteration{
          thisTest = error $ concat [
              "thisTest is undefined. "
            , "Use testMinimumForIteration "
            , "instead of testMinimum."
            ]
        }

-- | Generalization of 'testMinimum'
--
-- See 'testShrinkingForGeneration' for detailed discussion of the context.
testMinimumForIteration :: forall e.
     Show e
  => Context.Iteration
  -> Predicate '[e] -> Property' e () -> Property' String ()
testMinimumForIteration iteration p prop = do
    static <- Context.static <$> getContext

    let initContext :: Context
        initContext = Context static iteration Context.Initial

    st <- genWith (const Nothing) $ Gen.captureLocalTree
    case runGen (runProperty prop initContext) st of
      ((TestPassed (), _run), _shrunk) ->
        -- The property passed; nothing to test
        discard
      ((TestDiscarded, _run), _shrunk) ->
        -- The property needs to be discarded; discard this one, too
        discard
      ((TestFailed initErr, initRun), shrunk) -> do
        let explanation :: ShrinkExplanation (Counterexample e) (Maybe (), TestRun)
            explanation =
                shrinkFrom
                  static
                  iteration
                  (\ctx -> resultIsValidShrink (Context.execution ctx) <$>
                             runProperty prop ctx)
                  st
                  (Counterexample Context.Initial initErr initRun, shrunk)

            minExample :: Counterexample e
            mRejected  :: Maybe [(Maybe (), TestRun)]
            (minExample, mRejected) = shrinkOutcome explanation

            rejected :: [TestRun]
            rejected  = maybe [] (map snd) mRejected

        case P.eval $ p .$ ("minimum", counterexampleError minExample) of
          Right () -> do
            -- For a successful test, we add the full shrink history as info
            -- This means that users can use verbose mode to see precisely
            -- how the minimum value is reached, if they wish.
            info "Shrink history:"
            forM_ (shrinkHistory explanation) $ \example ->
              info $ show (counterexampleError example)
          Left err -> do
            appendLog (runLog $ counterexampleRun minExample)
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
testGen' p g = WrapProperty $ TestResultT $ ReaderT $ \_ctx -> StateT $ \run ->
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
--
-- See also 'testShrinkingOfGenForIteration'.
testShrinkingOfGen :: Show a => Predicate [a, a] -> Gen a -> Property' String ()
testShrinkingOfGen =
    testShrinkingOfGenForIteration $ Context.Iteration{
          thisTest = error $ concat [
              "thisTest is undefined. "
            , "Use testShrinkingOfGenForIteration "
            , "instead of testShrinkingOfGen."
            ]
        }

-- | Generalization of 'testShrinkingOfGen'
--
-- See 'testShrinkingForGeneration' for detailed discussion of the context.
testShrinkingOfGenForIteration ::
     Show a
  => Context.Iteration
  -> Predicate [a, a] -> Gen a -> Property' String ()
testShrinkingOfGenForIteration iteration p =
    testShrinkingForIteration iteration p . testGen' Left
