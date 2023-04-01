{-# OPTIONS_GHC -Wno-orphans #-}
-- | Tasty integration
--
-- This are the internal guts of the integration. Publicly visible API lives in
-- "Test.Tasty.Falsify".
module Test.Falsify.Internal.Tasty (
    -- * Test property
    testProperty
    -- * Configure test behaviour
  , TestOptions(..)
  , Verbose(..)
  , ExpectFailure(..)
  , testPropertyWith
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

data Test = Test TestOptions (Property' String ())

data TestOptions = TestOptions {
      expectFailure      :: ExpectFailure
    , overrideVerbose    :: Maybe Verbose
    , overrideMaxShrinks :: Maybe Word
    , overrideNumTests   :: Maybe Word
    , overrideMaxRatio   :: Maybe Word
    }

instance Default TestOptions where
  def = TestOptions {
        expectFailure      = DontExpectFailure
      , overrideVerbose    = Nothing
      , overrideMaxShrinks = Nothing
      , overrideNumTests   = Nothing
      , overrideMaxRatio   = Nothing
      }

instance IsTest Test where
  -- @tasty@ docs (1.4.3) explicitly say to ignore the @reportProgress@ argument
  run opts (Test testOpts prop) _reportProgress =
      toTastyResult . renderTestResult verbose (expectFailure testOpts) <$>
        falsify driverOpts prop
    where
      verbose :: Verbose
      verbose = fromMaybe (Tasty.lookupOption opts) (overrideVerbose testOpts)

      driverOpts :: Driver.Options
      driverOpts =
            maybe id
              (\x o -> o{Driver.maxShrinks = Just x})
              (overrideMaxShrinks testOpts)
          $ maybe id
              (\x o -> o{Driver.tests = x})
              (overrideNumTests testOpts)
          $ maybe id
              (\x o -> o{Driver.maxRatio = x})
              (overrideMaxRatio testOpts)
          $ driverOptions opts

  testOptions = Tagged [
        Tasty.Option $ Proxy @Verbose
      , Tasty.Option $ Proxy @Tests
      , Tasty.Option $ Proxy @MaxShrinks
      , Tasty.Option $ Proxy @Replay
      , Tasty.Option $ Proxy @MaxRatio
      ]

toTastyResult :: RenderedTestResult -> Tasty.Result
toTastyResult RenderedTestResult{testPassed, testOutput}
  | testPassed = Tasty.testPassed testOutput
  | otherwise  = Tasty.testFailed testOutput

{-------------------------------------------------------------------------------
  User API
-------------------------------------------------------------------------------}

-- | Generalization of 'testPropertyWith' using default options
testProperty :: TestName -> Property' String () -> TestTree
testProperty = testPropertyWith def

testPropertyWith :: TestOptions -> TestName -> Property' String () -> TestTree
testPropertyWith testOpts name = Tasty.singleTest name . Test testOpts

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

  NOTE: If we add another option here, we must also add it in 'testOptions'.
-------------------------------------------------------------------------------}

newtype Tests      = Tests      { getTests      :: Word             }
newtype MaxShrinks = MaxShrinks { getMaxShrinks :: Maybe Word       }
newtype Replay     = Replay     { getReplay     :: Maybe ReplaySeed }
newtype MaxRatio   = MaxRatio   { getMaxRatio   :: Word             }

instance IsOption Tests where
  defaultValue   = Tests (Driver.tests def)
  parseValue     = fmap Tests . Tasty.safeRead . filter (/= '_')
  optionName     = Tagged "falsify-tests"
  optionHelp     = Tagged "Number of test cases to generate"

instance IsOption MaxShrinks where
  defaultValue   = MaxShrinks (Driver.maxShrinks def)
  parseValue     = fmap (MaxShrinks . Just) . Tasty.safeRead
  optionName     = Tagged "falsify-shrinks"
  optionHelp     = Tagged "Random seed to use for replaying a previous test run"

instance IsOption Replay where
  defaultValue   = Replay (Driver.replay def)
  parseValue     = fmap (Replay . Just) . safeReadReplaySeed
  optionName     = Tagged "falsify-replay"
  optionHelp     = Tagged "Random seed to use for replaying test"
  optionCLParser = Opts.option readReplaySeed $ mconcat [
                       Opts.long $ untag $ optionName @Replay
                     , Opts.help $ untag $ optionHelp @Replay
                     ]
    where
      readReplaySeed :: Opts.ReadM Replay
      readReplaySeed = Opts.str >>= fmap (Replay . Just) . parseReplaySeed

instance IsOption MaxRatio where
  defaultValue   = MaxRatio (Driver.maxRatio def)
  parseValue     = fmap MaxRatio . Tasty.safeRead . filter (/= '_')
  optionName     = Tagged "falsify-max-ratio"
  optionHelp     = Tagged "Maximum number of discarded tests per successful test"

driverOptions :: OptionSet -> Driver.Options
driverOptions opts = Driver.Options {
      tests         = getTests      $ Tasty.lookupOption opts
    , maxShrinks    = getMaxShrinks $ Tasty.lookupOption opts
    , replay        = getReplay     $ Tasty.lookupOption opts
    , maxRatio      = getMaxRatio   $ Tasty.lookupOption opts
    }
