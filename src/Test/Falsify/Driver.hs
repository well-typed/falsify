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
  ) where

import Prelude hiding (log)

import Data.Default
import System.Random.SplitMix

import Test.Falsify.Debugging
import Test.Falsify.Driver.ReplaySeed
import Test.Falsify.Property
import Test.Falsify.SampleTree (SampleTree)

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.SampleTree as SampleTree
import Data.Bifunctor

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
    , successLog  :: Log
    }

data Failure = Failure {
      failureSeed :: ReplaySeed
    , failureLog  :: ShrinkExplanation (String, Log) Log
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
            log     :: Log
            (outcome, log) = Gen.run (runProperty prop) st

        case outcome of

          -- Test passed
          Right () -> do
            let success :: Success
                success = Success {
                    successSeed = splitmixReplaySeed now
                  , successLog  = log
                  }
            go later (success : acc) (pred n)

          -- Test failed
          --
          -- We ignore the failure message here, because this is the failure
          -- message before shrinking, which we are typically not interested in.
          Left _-> do
            let explanation :: ShrinkExplanation (String, Log) Log
                explanation = limitShrinkSteps (maxShrinks opts) . second snd $
                    shrinkExplain shortcutMinimal isValid (runProperty prop) st

                failure :: Failure
                failure = Failure {
                      failureSeed = splitmixReplaySeed now
                    , failureLog  = explanation
                    }

            return (acc, Just failure)

    -- It's a valid shrink step if the test still fails
    isValid :: (Either e a, Log) -> IsValidShrink (e, Log) (a, Log)
    isValid (Left  e, log) = ValidShrink   (e, log)
    isValid (Right a, log) = InvalidShrink (a, log)
