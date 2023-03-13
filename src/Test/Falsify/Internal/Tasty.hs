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
import Data.List (intercalate)
import Data.Maybe
import Data.Proxy
import Data.Tagged
import GHC.Stack
import Test.Tasty.Options
import Test.Tasty.Providers hiding (Result)

import qualified Data.List.NonEmpty as NE

import Test.Falsify.Debugging
import Test.Falsify.Driver (Success, Failure, falsify)
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
      toTastyResult verbose (expectFailure testOpts) <$>
        falsify driverOpts prop
    where
      verbose :: Verbose
      verbose = fromMaybe (lookupOption opts) (overrideVerbose testOpts)

      driverOpts :: Driver.Options
      driverOpts =
            maybe id
              (\x o -> o{Driver.maxShrinks = Just x})
              (overrideMaxShrinks testOpts)
          $ driverOptions opts

  testOptions = Tagged [
        Option $ Proxy @Tests
      , Option $ Proxy @Verbose
      , Option $ Proxy @MaxShrinks
      ]

{-------------------------------------------------------------------------------
  Pretty-printing for verbose mode

  TODO: Not all modes are supported yet; in particular, not all verbose modes
  are implemented yet.

  TODO: These currently create quite ugly output, they definitely could be
  improved.
-------------------------------------------------------------------------------}

toTastyResult ::
     Verbose
  -> ExpectFailure
  -> (ReplaySeed, [Success], Maybe Failure)
  -> Tasty.Result
toTastyResult verbose expectFailure (initSeed, successes, mFailure) =
    case (verbose, expectFailure, mFailure) of
      -- We weren't expecting a failure, and didn't get one: test succeeds
      (NotVerbose, DontExpectFailure, Nothing) ->
        testPassed $
          if length successes == 1
            then "Passed 1 test"
            else concat ["Passed "
                   , show (length successes)
                   , " tests"
                   ]
      (Verbose, DontExpectFailure, Nothing) ->
        testPassed $ intercalate "\n" [
            if length successes == 1
              then "Passed 1 test"
              else concat ["Passed "
                     , show (length successes)
                     , " tests"
                     ]
          , "Full logs:"
          , intercalate "\n" $ map renderSuccess (zip [1..] successes)
          ]

      -- We weren't expecting a failure, but did get one: test fails
      (NotVerbose, DontExpectFailure, Just e) ->
        let history = shrinkHistory (Driver.failureRun e) in
        testFailed $ intercalate "\n" [
            concat [
                "Failed after "
              , show (length successes)
              , " successful tests and "
              , show (length history - 1)
              , " shrinks: "
              , fst $ NE.last history
              ]
          , concat [
                "Replay-seed: "
              , show (Driver.failureSeed e)
              ]
          , "Full log:"
          , renderLog $ runLog $ snd (NE.last history)
          ]

      -- We were expecting failure, but didn't get one: test fails
      (NotVerbose, ExpectFailure, Nothing) ->
        testFailed $ intercalate "\n" [
            concat [
                "Expected failure, but all "
              , show (length successes)
              , " passed"
              ]
          , concat [
                "Replay-seed: "
              , show initSeed
              ]
          ]

      -- We were expecting failure, and got it: test succeeds
      (NotVerbose, ExpectFailure, Just e) ->
        let history = shrinkHistory (Driver.failureRun e) in
        testPassed $ concat [
              "Found expected failure after "
            , show (length successes)
            , " successful tests and "
            , show (length history - 1)
            , " shrinks: "
            , fst $ NE.last history
            ]

      _otherwise ->
        error "TODO"

renderSuccess :: (Int, Success) -> String
renderSuccess (ix, Driver.Success{successRun}) =
    intercalate "\n" . concat $ [
        ["Test " ++ show ix]
      , ["Logs:"]
      , [renderLog $ runLog successRun]
      ]

renderLog :: Log -> String
renderLog (Log log) = intercalate "\n" $ map renderLogEntry (reverse log)

renderLogEntry :: LogEntry -> String
renderLogEntry = \case
    Generated stack x -> aux stack ("generated " ++ x)
    Info      stack x -> aux stack x
  where
    aux :: CallStack -> String -> String
    aux stack x = x ++ " at " ++ prettyCallStack stack

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
testPropertyWith testOpts name =
      singleTest name
    . Test testOpts

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

-- | Verbose output
data Verbose = Verbose | NotVerbose

-- | Do we expect the property to fail?
--
-- If 'ExpectFailure', the test will fail if the property does /not/ fail. The
-- interpretation of 'tests' is now different when 'expectFailure': it suffices
-- to find a /single/ failing test, so 'tests' becomes a maximum instead.
data ExpectFailure = ExpectFailure | DontExpectFailure

instance IsOption Verbose where
  defaultValue   = NotVerbose
  parseValue     = fmap (\b -> if b then Verbose else NotVerbose) . safeReadBool
  optionName     = Tagged $ "falsify-verbose"
  optionHelp     = Tagged $ "Show the generated test cases"
  optionCLParser = mkFlagCLParser mempty Verbose

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

newtype Tests      = Tests      { getTests      :: Word             }
newtype MaxShrinks = MaxShrinks { getMaxShrinks :: Maybe Word       }
newtype Replay     = Replay     { getReplay     :: Maybe ReplaySeed }

instance IsOption Tests where
  defaultValue   = Tests (Driver.tests def)
  parseValue     = fmap Tests . safeRead . filter (/= '_')
  optionName     = Tagged $ "falsify-tests"
  optionHelp     = Tagged $ "Number of test cases to generate"

instance IsOption MaxShrinks where
  defaultValue   = MaxShrinks (Driver.maxShrinks def)
  parseValue     = fmap (MaxShrinks . Just) . safeRead
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
      tests         = getTests      $ lookupOption opts
    , maxShrinks    = getMaxShrinks $ lookupOption opts
    , replay        = getReplay     $ lookupOption opts
    }
