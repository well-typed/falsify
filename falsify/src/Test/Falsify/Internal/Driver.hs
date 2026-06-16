-- | Test driver
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Internal.Driver (Success, Failure, falsify)
-- > import qualified Test.Falsify.Internal.Driver as Driver
module Test.Falsify.Internal.Driver (
    -- * Options
    Options(..)
    -- * Results
  , Success(..)
  , Failure(..)
  , TestOutcome'(..)
  , TestOutcome
    -- * Test driver
  , falsify
    -- * Process results
  , Verbose(..)
  , ExpectFailure(..)
  , RenderedTestOutcome(..)
  , renderTestOutcome
  ) where

import Prelude hiding (log)

import Data.Bifunctor
import Data.Default
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Exception
import System.Random.SplitMix
import Text.Printf

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set           as Set

import Test.Falsify.Context (Context(Context))
import Test.Falsify.Internal.Driver.ReplaySeed
import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Property
import Test.Falsify.Internal.Shrinking
import Test.Falsify.SampleTree (SampleTree)

import qualified Test.Falsify.Context    as Context
import qualified Test.Falsify.SampleTree as SampleTree

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Options for running a test
data Options = Options {
      -- | Number of test cases to generate
      tests :: Word

      -- | Number of shrinks allowed before failing a test
    , maxShrinks :: Maybe Word

      -- | Random seed to use for replaying a previous test run
    , replay :: Maybe ReplaySeed

      -- | Maximum number of discarded test per successful test
    , maxRatio :: Word
    }

instance Default Options where
  def = Options {
        tests      = 100
      , maxShrinks = Nothing
      , replay     = Nothing
      , maxRatio   = 100
      }

{-------------------------------------------------------------------------------
  Driver
-------------------------------------------------------------------------------}

data Success a = Success {
      successIteration :: Context.Iteration
    , successResult    :: a
    , successSeed      :: ReplaySeed
    , successRun       :: TestRun
    }
  deriving (Show)

data Failure e = Failure {
      failureSeed :: ReplaySeed
    , failureRun  :: ShrinkExplanation (Counterexample e) TestRun
    }
  deriving (Show)

-- | Result of running @falsify@
--
-- This is an opaque type; see 'renderTestOutcome' and the additional accessors
-- provided in "Test.Falsify.Driver".
data TestOutcome' e a = TestOutcome{
      -- | Initial replay seed (each test also records its own seed)
      testReplaySeed :: ReplaySeed

      -- | Successful tests
    , testSuccesses :: [Success a]

      -- | Number of discarded tests
    , testDiscarded :: Word

      -- | Failed test (if any)
    , testFailure :: Maybe (Failure e)
    }

-- | t'TestOutcome' specialized to 'String' for errors
--
-- This mimicks the 'Property'' vs 'Property' distinction.
type TestOutcome = TestOutcome' String

-- | Run a test: attempt to falsify the given property
falsify :: forall e a. Options -> Property' e a -> IO (TestOutcome' e a)
falsify opts prop = do
    acc <- initDriverState opts
    (successes, discarded, mFailure) <- go acc
    return TestOutcome{
        testReplaySeed = splitmixReplaySeed (prng acc)
      , testSuccesses  = successes
      , testDiscarded  = discarded
      , testFailure    = mFailure
      }
  where
    static :: Context.Static
    static = Context.Static{
          tests      = tests      opts
        , maxShrinks = maxShrinks opts
        , maxRatio   = maxRatio   opts
        }

    go :: DriverState a -> IO ([Success a], Word, Maybe (Failure e))
    go acc | thisTest acc > tests opts = return (
          reverse $ successes acc
        , discardedTotal acc
        , Nothing
        )
    go acc = do
        let iteration :: Context.Iteration
            iteration = Context.Iteration{
                  thisTest = thisTest acc
                }

            initContext :: Context
            initContext = Context static iteration Context.Initial

            now, later :: SMGen
            (now, later) = splitSMGen (prng acc)

            st :: SampleTree
            st = SampleTree.fromPRNG now

            result :: TestResult e a
            run    :: TestRun
            shrunk :: [SampleTree]
            ((result, run), shrunk) = runGen (runProperty prop initContext) st

        case result of
          -- Test passed
          TestPassed x -> do
            let success :: Success a
                success = Success {
                    successIteration = iteration
                  , successResult    = x
                  , successSeed      = splitmixReplaySeed now
                  , successRun       = run
                  }
            if runDeterministic run then
              case (successes acc, discardedTotal acc) of
                ([], 0)    -> return ([success], 0, Nothing)
                _otherwise -> error "falsify.go: impossible"
            else
              go $ withSuccess later success acc

          -- Test failed
          --
          -- We ignore the failure message here, because this is the failure
          -- message before shrinking, which we are typically not interested in.
          TestFailed e -> do
            let explanation :: ShrinkExplanation (Counterexample e) TestRun
                explanation =
                    second snd $
                      shrinkFrom
                        static
                        iteration
                        ( \ctx ->
                             resultIsValidShrink (Context.execution ctx) <$>
                               runProperty prop ctx
                        )
                        st
                        (Counterexample Context.Initial e run, shrunk)

                -- We have to be careful here: if the user specifies a seed, we
                -- will first /split/ it to run the test (call to splitSMGen,
                -- above). This means that the seed we should provide for the
                -- test is the seed /before/ splitting.
                failure :: Failure e
                failure = Failure {
                      failureSeed = splitmixReplaySeed (prng acc)
                    , failureRun  = explanation
                    }

            return (successes acc, discardedTotal acc, Just failure)

          -- Test discarded, but reached maximum already
          TestDiscarded | discardedForTest acc == maxRatio opts ->
            return (successes acc, discardedTotal acc, Nothing)

          -- Test discarded; continue.
          TestDiscarded ->
            go $ withDiscard later acc

{-------------------------------------------------------------------------------
  Internal: driver state
-------------------------------------------------------------------------------}

data DriverState a = DriverState {
      -- | State of the PRNG after the previously executed test
      prng :: SMGen

      -- | Accumulated successful tests
    , successes :: [Success a]

      -- | Number of tests we discarded so far (for this test)
    , discardedForTest :: Word

      -- | Number of tests we discarded (in total)
    , discardedTotal :: Word

      -- | Current test number
    , thisTest :: Word
    }
  deriving (Show)

initDriverState :: Options -> IO (DriverState a)
initDriverState opts = do
    prng <- case replay opts of
              Just ReplaySeed{replaySeed, replayGamma} ->
                return $ seedSMGen replaySeed replayGamma
              Nothing ->
                initSMGen
    return $ DriverState {
        prng
      , successes        = []
      , discardedForTest = 0
      , discardedTotal   = 0
      , thisTest         = 1
      }

withSuccess :: SMGen -> Success a -> DriverState a -> DriverState a
withSuccess next success acc = DriverState {
      prng             = next
    , successes        = success : successes acc
    , discardedForTest = 0 -- reset for the next test
    , discardedTotal   = discardedTotal acc
    , thisTest         = succ (thisTest acc)
    }

withDiscard :: SMGen -> DriverState a -> DriverState a
withDiscard next acc = DriverState {
      prng             = next
    , successes        = successes acc
    , discardedForTest = succ $ discardedForTest acc
    , discardedTotal   = succ $ discardedTotal acc
    , thisTest         = thisTest acc
    }

{-------------------------------------------------------------------------------
  Process results
-------------------------------------------------------------------------------}

-- | Verbose output
--
-- Note that if a test fails (and we were not expecting failure) we show the
-- logs independent of verbosity.
data Verbose = Verbose | NotVerbose

-- | Do we expect the property to fail?
--
-- If v'ExpectFailure', the test will fail if the property does /not/ fail.
-- Note that if we expect failure for a property, then we can stop at the first
-- failed test; the number of tests to run for the property becomes a maximum
-- rather than a goal.
data ExpectFailure = ExpectFailure | DontExpectFailure

-- | Test outcome as it should be shown to the user
--
-- The rendered test outcome can usually be used directly in test framework
-- integration. For example, the @tasty@ integration uses
--
-- > toTastyResult :: RenderedTestOutcome -> Tasty.Result
-- > toTastyResult RenderedTestOutcome{testPassed, testOutput}
-- >   | testPassed = Tasty.testPassed testOutput
-- >   | otherwise  = Tasty.testFailed testOutput
data RenderedTestOutcome = RenderedTestOutcome {
      testPassed :: Bool
    , testOutput :: String
    }

-- | Render test outcome
--
-- See t'RenderedTestOutcome' for discussion.
renderTestOutcome ::
     Verbose
  -> ExpectFailure
  -> TestOutcome ()
  -> RenderedTestOutcome
renderTestOutcome
      verbose
      expectFailure
      (TestOutcome initSeed successes discarded mFailure) =
    case (verbose, expectFailure, mFailure) of

      --
      -- All tests discarded
      --
      -- TODO: Verbose mode here does nothing currently (we get no logs for
      -- discarded tests).
      --

      (_, DontExpectFailure, Nothing) | null successes -> RenderedTestOutcome {
            testPassed = False
          , testOutput = unlines [
                concat [
                    "All tests discarded"
                  , countDiscarded
                  ]
              ]
          }

      --
      -- Test succeeded
      --
      -- This may still be a failure, if we were expecting the test not to
      -- succeed.
      --

      (NotVerbose, DontExpectFailure, Nothing) -> RenderedTestOutcome {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     countSuccess
                   , countDiscarded
                   ]
               , showLabels
               ]
           }

      (Verbose, DontExpectFailure, Nothing) -> RenderedTestOutcome {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     countSuccess
                   , countDiscarded
                   ]
               , ""
               , "Logs for each test run below."
               , ""
               , unlines $ map renderSuccess successes
               ]
           }

      (NotVerbose, ExpectFailure, Nothing) -> RenderedTestOutcome {
             testPassed = False
           , testOutput = unlines [
                 "Expected failure, but " ++ countAll ++ " passed"
               , showSeed initSeed
               ]
           }

      (Verbose, ExpectFailure, Nothing) -> RenderedTestOutcome {
             testPassed = False
           , testOutput = unlines [
                 "Expected failure, but " ++ countAll ++ " passed"
               , ""
               , "Logs for each test run below."
               , ""
               , intercalate "\n" $ map renderSuccess successes
               , showSeed initSeed
               ]
           }

      --
      -- Test failed
      --
      -- This might still mean the test passed, if we /expected/ failure.
      --
      -- If the test failed and we were not expecting failure, we show the
      -- logs independent of verbosity.
      --

      (NotVerbose, ExpectFailure, Just e) -> RenderedTestOutcome {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     "expected failure after "
                   , countHistory history
                   , countDiscarded
                   ]
               , counterexampleError $ NE.last history
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (Verbose, ExpectFailure, Just e) -> RenderedTestOutcome {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     "expected failure after "
                   , countHistory history
                   , countDiscarded
                   ]
               , counterexampleError $ NE.last history
               , "Logs for failed test run:"
               , renderLog . runLog . counterexampleRun $ NE.last history
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (NotVerbose, DontExpectFailure, Just e) -> RenderedTestOutcome {
             testPassed = False
           , testOutput = unlines [
                 "failed after " ++ countHistory history
               , counterexampleError $ NE.last history
               , "Logs for failed test run:"
               , renderLog . runLog . counterexampleRun $ NE.last history
               , showSeed $ failureSeed e
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (Verbose, DontExpectFailure, Just e) -> RenderedTestOutcome {
             testPassed = False
           , testOutput = unlines [
                 "failed after " ++ countHistory history
               , counterexampleError $ NE.last history
               , ""
               , "Logs for complete shrink history:"
               , ""
               , intercalate "\n" $ [
                     intercalate "\n" [
                         showStep (counterexampleContext example)
                       , renderLog (runLog $ counterexampleRun example)
                       ]
                   | example <- NE.toList history
                   ]
               , showSeed $ failureSeed e
               ]
           }
         where
           history = shrinkHistory (failureRun e)
  where
    countSuccess, countDiscarded, countAll :: String
    countSuccess
      | length successes == 1 = "1 successful test"
      | otherwise             = show (length successes) ++ " successful tests"
    countDiscarded
      | discarded == 0        = ""
      | otherwise             = " (discarded " ++ show discarded ++ ")"
    countAll
      | length successes == 1 = "the test"
      | otherwise             = "all " ++ show (length successes) ++ " tests"

    -- The history includes the original value, so the number of shrink steps
    -- is the length of the history minus 1.
    countHistory :: NonEmpty (Counterexample String) -> [Char]
    countHistory history = concat [
          if | length successes == 0 -> ""
             | otherwise             -> countSuccess ++ " and "
        , if | numShrinks == 1 -> "1 shrink"
             | otherwise       -> show numShrinks ++ " shrinks"
        ]
      where
        numShrinks :: Word
        numShrinks =
            case counterexampleContext $ NE.last history of
              Context.Final i ->
                -- Under normal circumstances this is the only expected case
                i
              Context.Initial ->
                -- No shrinking steps at all
                0
              Context.Shrinking i ->
                -- @i@ here is the index of the step, not the number of steps
                i + 1


    showSeed :: ReplaySeed -> String
    showSeed seed = "Use --falsify-replay=" ++ show seed ++ " to replay."

    showLabels :: String
    showLabels = intercalate "\n" [
          intercalate "\n" $ ("\nLabel " ++ show l ++ ":") : [
              asPct n ++ " " ++ v
            | v <- Set.toList (Map.findWithDefault Set.empty l allValues)
            , let n = Map.findWithDefault 0         v
                    $ Map.findWithDefault Map.empty l
                    $ perTest
            ]
        | l <- Set.toList allLabels
        ]
      where
        -- Absolute number of tests as a percentage of total successes
        asPct :: Int -> String
        asPct n =
           printf "  %8.4f%%" pct
          where
            pct :: Double
            pct = fromIntegral n / fromIntegral (length successes) * 100

        -- All labels across all tests
        allLabels :: Set String
        allLabels = Map.keysSet allValues

        -- For each label, all values reported across all tests
        allValues :: Map String (Set String)
        allValues =
            Map.unionsWith Set.union $
              map (runLabels . successRun) successes

        -- For each label and each value, the corresponding number of tests
        perTest :: Map String (Map String Int)
        perTest =
            Map.fromList [
                (l, Map.fromList [
                    (v, length $ filter (labelHasValue l v) successes)
                  | v <- Set.toList $
                             Map.findWithDefault Set.empty l allValues
                  ])
              | l <- Set.toList allLabels
              ]

        -- Check if in particular test run label @l@ has value @v@
        labelHasValue :: String -> String -> Success () -> Bool
        labelHasValue l v =
              Set.member v
            . Map.findWithDefault Set.empty l
            . runLabels
            . successRun

    showStep :: Context.Execution -> [Char]
    showStep = \case
        Context.Initial ->
          "Initial counter-example"
        Context.Shrinking i ->
          "Shrinking step " ++ show i
        Context.Final i ->
          "Final counter-example after " ++ show i ++ " shrink steps"

renderSuccess :: Success () -> String
renderSuccess Success{successIteration, successRun} =
    intercalate "\n" . concat $ [
        ["Test " ++ show (Context.thisTest successIteration)]
      , [renderLog $ runLog successRun]
      ]

renderLog :: Log -> String
renderLog (Log log) = unlines $ map renderLogEntry (reverse log)

renderLogEntry :: LogEntry -> String
renderLogEntry = \case
    Generated stack x -> concat [
        "generated "
      , x
      , " at "
      , prettyCallStack stack
      ]
    Info x -> x
