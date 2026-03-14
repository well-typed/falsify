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
  , TotalDiscarded(..)
    -- * Test driver
  , falsify
  , falsifyIO
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
import Data.Map (Map)
import Data.Set (Set)
import GHC.Exception
import System.Random.SplitMix
import Text.Printf

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set           as Set

import Test.Falsify.Internal.Driver.ReplaySeed
import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Generator.Shrinking
import Test.Falsify.Internal.Property
import Test.Falsify.Internal.SampleTree (SampleTree)

import qualified Test.Falsify.Internal.SampleTree as SampleTree
import Control.Exception (try, AsyncException, throwIO)
import Control.Monad (when)
import Data.Maybe (isJust)
import Debug.Trace (trace)

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
      successResult :: a
    , successSeed   :: ReplaySeed
    , successRun    :: TestRun
    }
  deriving (Show)

data Failure e = Failure {
      failureSeed :: ReplaySeed
    , failureRun  :: ShrinkExplanation (e, TestRun) TestRun
    }
  deriving (Show)

newtype TotalDiscarded = TotalDiscarded Word

-- | Run a test: attempt to falsify the given property
--
-- We return
--
-- * initial replay seed (each test also records its own seed)
-- * successful tests
-- * how many tests we discarded
-- * the failed test (if any).
falsify :: forall e a.
     Options
  -> Property' e a
  -> IO (ReplaySeed, [Success a], TotalDiscarded, Maybe (Failure e))
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
    go :: DriverState a -> IO ([Success a], Word, Maybe (Failure e))
    go acc | todo acc == 0 = return (successes acc, discardedTotal acc, Nothing)
    go acc = do
        let now, later :: SMGen
            (now, later) = splitSMGen (prng acc)

            st :: SampleTree
            st = SampleTree.fromPRNG now

            result :: TestResult e a
            run    :: TestRun
            shrunk :: [SampleTree]
            ((result, run), shrunk) = runGen (runProperty prop) st

        case result of
          -- Test passed
          TestPassed x -> do
            let success :: Success a
                success = Success {
                    successResult = x
                  , successSeed   = splitmixReplaySeed now
                  , successRun    = run
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
            let explanation :: ShrinkExplanation (e, TestRun) TestRun
                explanation =
                    limitShrinkSteps (maxShrinks opts) . second snd $
                      shrinkFrom
                        resultIsValidShrink
                        (runProperty prop)
                        ((e, run), shrunk)

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

-- | Run a test: attempt to falsify the given property
--
-- We return
--
-- * initial replay seed (each test also records its own seed)
-- * successful tests
-- * how many tests we discarded
-- * the failed test (if any).
falsifyIO :: forall a.
     Options
  -> Property' String (IO a)
  -> IO (ReplaySeed, [Success a], TotalDiscarded, Maybe (Failure String))
falsifyIO opts prop = do
    acc <- initDriverState opts
    (successes, discarded, mFailure) <- go acc
    return (
        splitmixReplaySeed (prng acc)
      , successes
      , TotalDiscarded discarded
      , mFailure
      )
  where
    go :: DriverState a -> IO ([Success a], Word, Maybe (Failure String))
    go acc | todo acc == 0 = return (successes acc, discardedTotal acc, Nothing)
    go acc = do
        let now, later :: SMGen
            (now, later) = splitSMGen (prng acc)

            st :: SampleTree
            st = SampleTree.fromPRNG now

            result :: TestResult String (IO a)
            run    :: TestRun
            shrunk :: [SampleTree]
            ((result, run), shrunk) = runGen (runProperty prop) st


        case result of
          -- Test passed
          TestPassed xIO -> do
            -- TODO: catch relevant exception here
            try @SomeException xIO >>= \case
                Left someException -> do
                    when (isJust (fromException @AsyncException someException))
                        $ throwIO someException

                    let e = trace "displayException" $ displayException someException
                    explanation <-
                        limitShrinkSteps (maxShrinks opts) . second snd <$>
                            shrinkFromIO
                                resultIsValidShrinkIO
                                (runProperty prop)
                                ((e, run), shrunk)

                    -- We have to be careful here: if the user specifies a seed, we
                    -- will first /split/ it to run the test (call to splitSMGen,
                    -- above). This means that the seed we should provide for the
                    -- test is the seed /before/ splitting.
                    let failure :: Failure String
                        failure = Failure {
                            failureSeed = splitmixReplaySeed (prng acc)
                            , failureRun  = explanation
                        }



                    return (successes acc, discardedTotal acc, Just failure)

                Right result' -> do
                    let success :: Success a
                        success = Success
                            { successResult = result'
                            , successSeed   = splitmixReplaySeed now
                            , successRun    = run
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
                explanation <-
                    limitShrinkSteps (maxShrinks opts) . second snd <$>
                        shrinkFromIO
                            resultIsValidShrinkIO
                            (runProperty prop)
                            ((e, run), shrunk)

                -- We have to be careful here: if the user specifies a seed, we
                -- will first /split/ it to run the test (call to splitSMGen,
                -- above). This means that the seed we should provide for the
                -- test is the seed /before/ splitting.
                let failure :: Failure String
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

      -- | Number of tests still to execute
    , todo :: Word

      -- | Number of tests we discarded so far (for this test)
    , discardedForTest :: Word

      -- | Number of tests we discarded (in total)
    , discardedTotal :: Word
    }
  deriving (Show)

initDriverState :: Options -> IO (DriverState a)
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

withSuccess :: SMGen -> Success a -> DriverState a -> DriverState a
withSuccess next success acc = DriverState {
      prng             = next
    , successes        = success : successes acc
    , todo             = pred (todo acc)
    , discardedForTest = 0 -- reset for the next test
    , discardedTotal   = discardedTotal acc
    }

withDiscard :: SMGen -> DriverState a -> DriverState a
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
  -> (ReplaySeed, [Success ()], TotalDiscarded, Maybe (Failure String))
  -> RenderedTestResult
renderTestResult
      verbose
      expectFailure
      (initSeed, successes, TotalDiscarded discarded, mFailure) =
    case (verbose, expectFailure, mFailure) of

      --
      -- All tests discarded
      --
      -- TODO: Verbose mode here does nothing currently (we get no logs for
      -- discarded tests).
      --

      (_, DontExpectFailure, Nothing) | null successes -> RenderedTestResult {
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

      (NotVerbose, DontExpectFailure, Nothing) -> RenderedTestResult {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     countSuccess
                   , countDiscarded
                   ]
               , showLabels
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
                   , countHistory history
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
                   , countHistory history
                   , countDiscarded
                   ]
               , fst $ NE.last history
               , "Logs for failed test run:"
               , renderLog . runLog . snd $ NE.last history
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (NotVerbose, DontExpectFailure, Just e) -> RenderedTestResult {
             testPassed = False
           , testOutput = unlines [
                 "failed after " ++ countHistory history
               , fst $ NE.last history
               , "Logs for failed test run:"
               , renderLog . runLog . snd $ NE.last history
               , showSeed $ failureSeed e
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (Verbose, DontExpectFailure, Just e) -> RenderedTestResult {
             testPassed = False
           , testOutput = unlines [
                 "failed after " ++ countHistory history
               , fst $ NE.last history
               , ""
               , "Logs for complete shrink history:"
               , ""
               , intercalate "\n" $ [
                     intercalate "\n" [
                         "Step " ++ show (step :: Word)
                       , renderLog (runLog run)
                       ]
                   | (step, (_result, run)) <- zip [1..] (NE.toList history)
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
    countHistory :: NonEmpty (String, TestRun) -> [Char]
    countHistory history = concat [
          if | length successes == 0 -> ""
             | otherwise             -> countSuccess ++ " and "
        , if | length history   == 2 -> "1 shrink"
             | otherwise             -> show (length history - 1) ++ " shrinks"
        ]

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

renderSuccess :: (Int, Success ()) -> String
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
