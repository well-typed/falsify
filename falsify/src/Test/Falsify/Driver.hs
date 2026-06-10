-- | The main @falsify@ driver
--
-- The main entrypoint into @falsify@ is the 'falsify' function, which attempts
-- to falsify the 'Test.Falsify.Internal.Property.Property'' it is given.
-- However, most users don't need to use this module directly; it is primarily
-- intended for integration of @falsify@ in test frameworks such as @tasty@.
-- For the case of @tasty@ /specifically/, see the @tasty-falsify@ package.
module Test.Falsify.Driver (
    falsify
    -- * Options
  , Options(..)
  , ReplaySeed -- opaque
  , ExpectFailure(..)
  , Verbose(..)
  , parseReplaySeed
    -- * Results
  , TestOutcome -- opaque
  , TestOutcome'
  , RenderedTestOutcome(..)
  , renderTestOutcome
    -- ** Additional accessors
  , successes
  , discarded
  , failure
  , failure'
  ) where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)

import qualified Data.List.NonEmpty as NE

import Test.Falsify.Internal.Driver
import Test.Falsify.Internal.Driver.ReplaySeed
import Test.Falsify.Internal.Shrinking

{-------------------------------------------------------------------------------
  Additional public accessors for 'TestOutcome'

  'TestOutcome' has quite a bit of @falsify@-internal detail; here we provide
  some accessors that may be useful in user code, without having to expose all
  of those internals.
-------------------------------------------------------------------------------}

-- | Successful test outcomes
--
-- NOTE: Normally the @a@ parameter for a top-level property is instantiated to
-- unit (@()@), in which case it's really only the /length/ of the list of
-- successes that is relevant.
successes :: forall e a. TestOutcome' e a -> [(ReplaySeed, a)]
successes TestOutcome{testSuccesses} = map aux testSuccesses
  where
    -- Drops the 'TestRun'
    aux :: Success a -> (ReplaySeed, a)
    aux Success{successSeed, successResult} = (successSeed, successResult)

-- | Number of discarded test
discarded :: TestOutcome' e a -> Word
discarded TestOutcome{testDiscarded} = testDiscarded

-- | Failing test, if any
--
-- Returns the error after shrinking
failure :: TestOutcome' e a -> Maybe (ReplaySeed, e)
failure = fmap (second NE.last) . failure'

-- | Generalization of 'failure' that returns the full shrinking history
failure' :: forall e a. TestOutcome' e a -> Maybe (ReplaySeed, NonEmpty e)
failure' TestOutcome{testFailure} = aux <$> testFailure
  where
    aux :: Failure e -> (ReplaySeed, NonEmpty e)
    aux Failure{failureSeed, failureRun} = (
          failureSeed
        ,   shrinkHistory
          $ first fst      -- Drop the 'TestRun's
          $ failureRun
        )