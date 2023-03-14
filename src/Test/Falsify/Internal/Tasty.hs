{-# OPTIONS_GHC -Wno-orphans #-}
-- | Tasty integration
--
-- This are the internal guts of the integration. Publicly visible API lives in
-- "Test.Tasty.Falsify".
module Test.Falsify.Internal.Tasty (
    -- * Test property
    testProperty
  , testShrinking
    -- * Configure test behaviour
  , TestOptions(..)
  , Verbose(..)
  , ExpectFailure(..)
  , testPropertyWith
  , testShrinkingWith
  ) where

import Prelude hiding (log)

import Data.Default
import Data.Maybe
import Data.Proxy
import Data.Tagged
import Test.Tasty
import Test.Tasty.Options (IsOption(..), OptionSet)
import Test.Tasty.Providers (IsTest(..))

import qualified Test.Tasty.Options as Tasty

import Test.Falsify.Driver
import Test.Falsify.Driver.ReplaySeed
import Test.Falsify.Internal.Property

import qualified Options.Applicative  as Opts
import qualified Test.Falsify.Driver  as Driver
import qualified Test.Tasty.Providers as Tasty

{-------------------------------------------------------------------------------
  Tasty integration
-------------------------------------------------------------------------------}

data Test = Test TestOptions (Property ())

data TestOptions = TestOptions {
      expectFailure      :: ExpectFailure
    , overrideVerbose    :: Maybe Verbose
    , overrideMaxShrinks :: Maybe Word
    }

instance Default TestOptions where
  def = TestOptions {
        expectFailure      = DontExpectFailure
      , overrideVerbose    = Nothing
      , overrideMaxShrinks = Nothing
      }

instance IsTest Test where
  -- @tasty@ docs (1.4.3) explicitly say to ignore the @reportProgress@ argument
  run opts (Test testOpts prop) _reportProgress =
      toTastyResult . testResult verbose (expectFailure testOpts) <$>
        falsify driverOpts prop
    where
      verbose :: Verbose
      verbose = fromMaybe (Tasty.lookupOption opts) (overrideVerbose testOpts)

      driverOpts :: Driver.Options
      driverOpts =
            maybe id
              (\x o -> o{Driver.maxShrinks = Just x})
              (overrideMaxShrinks testOpts)
          $ driverOptions opts

  testOptions = Tagged [
        Tasty.Option $ Proxy @Tests
      , Tasty.Option $ Proxy @Verbose
      , Tasty.Option $ Proxy @MaxShrinks
      ]

toTastyResult :: TestResult -> Tasty.Result
toTastyResult TestResult{testPassed, testOutput}
  | testPassed = Tasty.testPassed testOutput
  | otherwise  = Tasty.testFailed testOutput

{-------------------------------------------------------------------------------
  User API
-------------------------------------------------------------------------------}

-- | Generalization of 'testPropertyWith' using default options
testProperty ::
     TestName
  -> Property ()
  -> TestTree
testProperty = testPropertyWith def

testPropertyWith ::
     TestOptions
  -> TestName
  -> Property ()
  -> TestTree
testPropertyWith testOpts name = Tasty.singleTest name . Test testOpts

-- | Generalization of 'testShrinkingWith' using default options
testShrinking ::
     Show a
  => TestName
  -> (a -> a -> Bool)
  -> Property a
  -> TestTree
testShrinking = testShrinkingWith def

-- | Test shrinking
testShrinkingWith ::
     Show a
  => TestOptions
  -> TestName
  -> (a -> a -> Bool) -- ^ Property that should hold for any shrink step
  -> Property a       -- ^ Generator to test
  -> TestTree
testShrinkingWith testOpts name p =
      testPropertyWith testOpts name
    . shrinkProperty p

{-------------------------------------------------------------------------------
  Options specific to the tasty test runner

  Not all of these options are command line options; some are set on a
  test-by-test basis, such as 'ExpectFailure'.
-------------------------------------------------------------------------------}

instance IsOption Verbose where
  defaultValue   = NotVerbose
  parseValue     = fmap (\b -> if b then Verbose else NotVerbose)
                 . Tasty.safeReadBool
  optionName     = Tagged $ "falsify-verbose"
  optionHelp     = Tagged $ "Show the generated test cases"
  optionCLParser = Tasty.mkFlagCLParser mempty Verbose

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

newtype Tests      = Tests      { getTests      :: Word             }
newtype MaxShrinks = MaxShrinks { getMaxShrinks :: Maybe Word       }
newtype Replay     = Replay     { getReplay     :: Maybe ReplaySeed }

instance IsOption Tests where
  defaultValue   = Tests (Driver.tests def)
  parseValue     = fmap Tests . Tasty.safeRead . filter (/= '_')
  optionName     = Tagged $ "falsify-tests"
  optionHelp     = Tagged $ "Number of test cases to generate"

instance IsOption MaxShrinks where
  defaultValue   = MaxShrinks (Driver.maxShrinks def)
  parseValue     = fmap (MaxShrinks . Just) . Tasty.safeRead
  optionName     = Tagged $ "falsify-shrinks"
  optionHelp     = Tagged $ "Random seed to use for replaying a previous test run"

instance IsOption Replay where
  defaultValue   = Replay (Driver.replay def)
  parseValue     = fmap (Replay . Just) . safeReadReplaySeed
  optionName     = Tagged $ "falsify-replay"
  optionHelp     = Tagged $ "Random seed to use for replaying test"
  optionCLParser = Opts.option readReplaySeed $ mconcat [
                       Opts.long $ untag $ optionName @Replay
                     , Opts.help $ untag $ optionHelp @Replay
                     ]
    where
      readReplaySeed :: Opts.ReadM Replay
      readReplaySeed = Opts.str >>= fmap (Replay . Just) . parseReplaySeed

driverOptions :: OptionSet -> Driver.Options
driverOptions opts = Driver.Options {
      tests         = getTests      $ Tasty.lookupOption opts
    , maxShrinks    = getMaxShrinks $ Tasty.lookupOption opts
    , replay        = getReplay     $ Tasty.lookupOption opts
    }
