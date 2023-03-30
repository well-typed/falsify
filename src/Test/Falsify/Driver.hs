-- | Test driver
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Driver (Success, Failure, falsify)
-- > import qualified Test.Falsify.Driver as Driver
module Test.Falsify.Driver (
    -- * Options
    Options(..)
    -- * Results
  , Success(..)
  , Failure(..)
    -- * Test driver
  , falsify
    -- * Process results
  , Verbose(..)
  , ExpectFailure(..)
  , RenderedTestResult(..)
  , renderTestResult
  ) where

import Prelude hiding (log)

import Data.Bifunctor
import Data.Default
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import GHC.Exception
import System.Random.SplitMix

import qualified Data.List.NonEmpty as NE

import Test.Falsify.Driver.ReplaySeed
import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Generator.Shrinking
import Test.Falsify.Internal.Property
import Test.Falsify.SampleTree (SampleTree)

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

data Success = Success {
      successSeed :: ReplaySeed
    , successRun  :: TestRun
    }

data Failure = Failure {
      failureSeed :: ReplaySeed
    , failureRun  :: ShrinkExplanation (String, TestRun) TestRun
    }

newtype TotalDiscarded = TotalDiscarded Word

-- | Run a test: attempt to falsify the given property
--
-- We return
--
-- * initial replay seed (each test also records its own seed)
-- * successful tests
-- * how many tests we discarded
-- * the failed test (if any).
falsify ::
     Options
  -> Property ()
  -> IO (ReplaySeed, [Success], TotalDiscarded, Maybe Failure)
falsify opts prop = do
    acc <- initDriverState opts
    (successes, discarded, mFailure) <- go acc
    return (
        splitmixReplaySeed (prng acc)
      , successes
      , TotalDiscarded discarded
      , mFailure
      )
  where
    go :: DriverState -> IO ([Success], Word, Maybe Failure)
    go acc | todo acc == 0 = return (successes acc, discardedTotal acc, Nothing)
    go acc = do
        let now, later :: SMGen
            (now, later) = splitSMGen (prng acc)

            st :: SampleTree
            st = SampleTree.fromPRNG now

        case runGen (runProperty prop) st of
          -- Test passed
          ((TestPassed (), run), _truncated, _shrunk) -> do
            let success :: Success
                success = Success {
                    successSeed = splitmixReplaySeed now
                  , successRun  = run
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
          ((TestFailed e, run), _truncated, shrunk) -> do
            let explanation :: ShrinkExplanation (String, TestRun) TestRun
                explanation =
                    limitShrinkSteps (maxShrinks opts) . second snd $
                      shrinkFrom isValid (runProperty prop) ((e, run), shrunk)

                failure :: Failure
                failure = Failure {
                      failureSeed = splitmixReplaySeed now
                    , failureRun  = explanation
                    }

            return (successes acc, discardedTotal acc, Just failure)

          -- Test discarded, but reached maximum already
          ((TestDiscarded, _), _, _) | discardedForTest acc == maxRatio opts ->
            return (successes acc, discardedTotal acc, Nothing)

          -- Test discarded; continue.
          ((TestDiscarded, _), _, _) ->
            go $ withDiscard later acc

    -- It's a valid shrink step if the test still fails
    isValid ::
         (TestResult a, TestRun)
      -> IsValidShrink (String, TestRun) (Maybe a, TestRun)
    isValid (TestFailed e  , run) = ValidShrink   (e       , run)
    isValid (TestDiscarded , run) = InvalidShrink (Nothing , run)
    isValid (TestPassed a  , run) = InvalidShrink (Just a  , run)

{-------------------------------------------------------------------------------
  Internal: driver state
-------------------------------------------------------------------------------}

data DriverState = DriverState {
      -- | State of the PRNG after the previously executed test
      prng :: SMGen

      -- | Accumulated successful tests
    , successes :: [Success]

      -- | Number of tests still to execute
    , todo :: Word

      -- | Number of tests we discarded so far (for this test)
    , discardedForTest :: Word

      -- | Number of tests we discarded (in total)
    , discardedTotal :: Word
    }

initDriverState :: Options -> IO DriverState
initDriverState opts = do
    prng <- case replay opts of
              Just (ReplaySplitmix seed gamma) ->
                return $ seedSMGen seed gamma
              Nothing ->
                initSMGen
    return $ DriverState {
        prng
      , successes        = []
      , todo             = tests opts
      , discardedForTest = 0
      , discardedTotal   = 0
      }

withSuccess :: SMGen -> Success -> DriverState -> DriverState
withSuccess next success acc = DriverState {
      prng             = next
    , successes        = success : successes acc
    , todo             = pred (todo acc)
    , discardedForTest = 0 -- reset for the next test
    , discardedTotal   = discardedTotal acc
    }

withDiscard :: SMGen -> DriverState -> DriverState
withDiscard next acc = DriverState {
      prng             = next
    , successes        = successes acc
    , todo             = todo acc
    , discardedForTest = succ $ discardedForTest acc
    , discardedTotal   = succ $ discardedTotal acc
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
-- If 'ExpectFailure', the test will fail if the property does /not/ fail.
-- Note that if we expect failure for a property, then we can stop at the first
-- failed test; the number of tests to run for the property becomes a maximum
-- rather than a goal.
data ExpectFailure = ExpectFailure | DontExpectFailure

-- | Test result as it should be shown to the user
data RenderedTestResult = RenderedTestResult {
      testPassed :: Bool
    , testOutput :: String
    }

renderTestResult ::
     Verbose
  -> ExpectFailure
  -> (ReplaySeed, [Success], TotalDiscarded, Maybe Failure)
  -> RenderedTestResult
renderTestResult
      verbose
      expectFailure
      (initSeed, successes, TotalDiscarded discarded, mFailure) =
    case (verbose, expectFailure, mFailure) of

      --
      -- Test succeeded
      --
      -- This may still be a failure, if we were expecting the test not to
      -- succeed.
      --

      (NotVerbose, DontExpectFailure, Nothing) -> RenderedTestResult {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     countSuccess
                   , countDiscarded
                   ]
               ]
           }

      (Verbose, DontExpectFailure, Nothing) -> RenderedTestResult {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     countSuccess
                   , countDiscarded
                   ]
               , ""
               , "Logs for each test run below."
               , ""
               , unlines $ map renderSuccess (zip [1..] successes)
               ]
           }

      (NotVerbose, ExpectFailure, Nothing) -> RenderedTestResult {
             testPassed = False
           , testOutput = unlines [
                 "Expected failure, but " ++ countAll ++ " passed"
               , showSeed initSeed
               ]
           }

      (Verbose, ExpectFailure, Nothing) -> RenderedTestResult {
             testPassed = False
           , testOutput = unlines [
                 "Expected failure, but " ++ countAll ++ " passed"
               , ""
               , "Logs for each test run below."
               , ""
               , intercalate "\n" $ map renderSuccess (zip [1..] successes)
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

      (NotVerbose, ExpectFailure, Just e) -> RenderedTestResult {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     "expected failure after "
                   , countryHistory history
                   , countDiscarded
                   ]
               , fst $ NE.last history
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (Verbose, ExpectFailure, Just e) -> RenderedTestResult {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     "expected failure after "
                   , countryHistory history
                   , countDiscarded
                   ]
               , fst $ NE.last history
               , "Logs for failed test run:"
               , renderLog . runLog . snd $ NE.last history
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (_, DontExpectFailure, Just e) -> RenderedTestResult {
             testPassed = False
           , testOutput = unlines [
                 "failed after " ++ countryHistory history
               , fst $ NE.last history
               , "Logs for failed test run:"
               , renderLog . runLog . snd $ NE.last history
               , showSeed initSeed
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

    countryHistory :: NonEmpty (String, TestRun) -> [Char]
    countryHistory history = concat [
          if | length successes == 0 -> ""
             | otherwise             -> countSuccess ++ " and "
        , if | length history   == 1 -> "1 shrink"
             | otherwise             -> show (length history) ++ " shrinks"
        ]

    showSeed :: ReplaySeed -> String
    showSeed seed = "Use --falsify-replay=" ++ show seed ++ " to replay."

renderSuccess :: (Int, Success) -> String
renderSuccess (ix, Success{successRun}) =
    intercalate "\n" . concat $ [
        ["Test " ++ show ix]
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
