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
  , TestResult(..)
  , testResult
  ) where

import Prelude hiding (log)

import Data.Bifunctor
import Data.Default
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import GHC.Exception
import System.Random.SplitMix

import qualified Data.List.NonEmpty as NE

import Test.Falsify.Debugging
import Test.Falsify.Driver.ReplaySeed
import Test.Falsify.Internal.Property
import Test.Falsify.SampleTree (SampleTree)

import qualified Test.Falsify.Generator                     as Gen
import qualified Test.Falsify.Internal.Generator.ShrinkStep as Step
import qualified Test.Falsify.SampleTree                    as SampleTree

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
    }

instance Default Options where
  def = Options {
        tests      = 100
      , maxShrinks = Nothing
      , replay     = Nothing
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

-- | Run a test: attempt to falsify the given property
--
-- We return the initial replay seed (each test also records its own seed),
-- the successful tests, and the failed test, if any.
falsify :: Options -> Property () -> IO (ReplaySeed, [Success], Maybe Failure)
falsify opts prop = do
    prng <- case replay opts of
              Just (ReplaySplitmix seed gamma) ->
                return $ seedSMGen seed gamma
              Nothing ->
                initSMGen
    (successes, mFailure) <- go prng [] (tests opts)
    return (splitmixReplaySeed prng, successes, mFailure)
  where
    go ::
         SMGen     -- State of the PRNG after the previously executed test
      -> [Success] -- Accumulated successful tests
      -> Word      -- Number of tests still to execute
      -> IO ([Success], Maybe Failure)
    go _    acc 0 = return (acc, Nothing)
    go prng acc n = do
        let now, later :: SMGen
            (now, later) = splitSMGen prng

            st :: SampleTree
            st = SampleTree.fromPRNG now

        let outcome :: Either String ()
            run     :: TestRun
            (outcome, run) = Gen.run (runProperty prop) st

        case outcome of

          -- Test passed
          Right () -> do
            let success :: Success
                success = Success {
                    successSeed = splitmixReplaySeed now
                  , successRun  = run
                  }
            if runDeterministic run then
              case acc of
                []         -> return ([success], Nothing)
                _otherwise -> error "falsify.go: impossible"
            else
              go later (success : acc) (pred n)

          -- Test failed
          --
          -- We ignore the failure message here, because this is the failure
          -- message before shrinking, which we are typically not interested in.
          Left _-> do
            let explanation :: ShrinkExplanation (String, TestRun) TestRun
                explanation =
                    limitShrinkSteps (maxShrinks opts) . second snd $
                      shrinkExplain
                        Step.shortcutMinimal
                        isValid
                        (runProperty prop)
                        st

                failure :: Failure
                failure = Failure {
                      failureSeed = splitmixReplaySeed now
                    , failureRun  = explanation
                    }

            return (acc, Just failure)

    -- It's a valid shrink step if the test still fails
    isValid :: (Either e a, TestRun) -> IsValidShrink (e, TestRun) (a, TestRun)
    isValid (Left  e, run) = ValidShrink   (e, run)
    isValid (Right a, run) = InvalidShrink (a, run)


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
data TestResult = TestResult {
      testPassed :: Bool
    , testOutput :: String
    }

testResult ::
     Verbose
  -> ExpectFailure
  -> (ReplaySeed, [Success], Maybe Failure)
  -> TestResult
testResult verbose expectFailure (initSeed, successes, mFailure) =
    case (verbose, expectFailure, mFailure) of

      --
      -- Test succeeded
      --
      -- This may still be a failure, if we were expecting the test not to
      -- succeed.
      --

      (NotVerbose, DontExpectFailure, Nothing) -> TestResult {
             testPassed = True
           , testOutput = countSuccess
           }

      (Verbose, DontExpectFailure, Nothing) -> TestResult {
             testPassed = True
           , testOutput = unlines [
                 countSuccess
               , ""
               , "Logs for each test run below."
               , ""
               , unlines $ map renderSuccess (zip [1..] successes)
               ]
           }

      (NotVerbose, ExpectFailure, Nothing) -> TestResult {
             testPassed = False
           , testOutput = unlines [
                 "Expected failure, but " ++ countAll ++ " passed"
               , showSeed initSeed
               ]
           }

      (Verbose, ExpectFailure, Nothing) -> TestResult {
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

      (NotVerbose, ExpectFailure, Just e) -> TestResult {
             testPassed = True
           , testOutput = unlines [
                 "expected failure after " ++ countryHistory history
               , fst $ NE.last history
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (Verbose, ExpectFailure, Just e) -> TestResult {
             testPassed = True
           , testOutput = unlines [
                 "expected failure after " ++ countryHistory history
               , fst $ NE.last history
               , ""
               , "Logs for failed test run:"
               , renderLog . runLog . snd $ NE.last history
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (_, DontExpectFailure, Just e) -> TestResult {
             testPassed = False
           , testOutput = unlines [
                 "failed after " ++ countryHistory history
               , fst $ NE.last history
               , ""
               , "Logs for failed test run:"
               , renderLog . runLog . snd $ NE.last history
               , showSeed initSeed
               ]
           }
         where
           history = shrinkHistory (failureRun e)
  where
    countSuccess, countAll :: String
    countSuccess
      | length successes == 1 = "1 successful test"
      | otherwise             = show (length successes) ++ " successful tests"
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
    showSeed seed = "Replay-seed: " ++ show seed

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
      , show x
      , " at "
      , prettyCallStack stack
      ]
    Info x -> x
