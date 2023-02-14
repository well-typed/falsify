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

data Success a = Success {
      successSeed    :: ReplaySeed
    , successOutcome :: a
    , successLog     :: Log
    }

data Failure e a = Failure {
      failureSeed    :: ReplaySeed
    , failureOutcome :: ShrinkExplanation (e, Log) (a, Log)
    }

-- | Run a test: attempt to falsify the given property
--
-- We return the initial replay seed (each test also records its own seed),
-- the successful tests, and the failed test, if any.
falsify :: forall e a.
     Options
  -> Property e a
  -> IO (ReplaySeed, [Success a], Maybe (Failure e a))
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
         SMGen
      -> [Success a]  -- Accumulated successful tests
      -> Word         -- Number of tests still to execute
      -> IO ([Success a], Maybe (Failure e a))
    go _    acc 0 = return (acc, Nothing)
    go prng acc n = do
        let here, next :: SMGen
            (here, next) = splitSMGen prng

            st :: SampleTree
            st = SampleTree.fromPRNG here

        let outcome :: Either e a
            log     :: Log
            (outcome, log) = Gen.run (runProperty prop) st

        case outcome of
          Right x -> do
            let success :: Success a
                success = Success {
                    successSeed    = splitmixReplaySeed here
                  , successOutcome = x
                  , successLog     = log
                  }
            go next (success : acc) (pred n)
          Left _ -> do
            let explanation :: ShrinkExplanation (e, Log) (a, Log)
                explanation =
                    limitShrinkSteps (maxShrinks opts) $
                      shrinkExplain shortcutMinimal isValid (runProperty prop) st

                failure :: Failure e a
                failure = Failure {
                      failureSeed    = splitmixReplaySeed here
                    , failureOutcome = explanation
                    }

            return (acc, Just failure)

    -- It's a valid shrink step if the test still fails
    isValid :: (Either e a, Log) -> IsValidShrink (e, Log) (a, Log)
    isValid (Left  e, log) = ValidShrink   (e, log)
    isValid (Right a, log) = InvalidShrink (a, log)
