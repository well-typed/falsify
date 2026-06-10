-- | Test options
module Test.Tasty.Falsify.Options (
    TestOptions(..)
  , assembleFalsifyOptions
  ) where

import Data.Default
import Data.Maybe (fromMaybe)

import qualified Test.Tasty.Options  as Tasty
import qualified Test.Falsify.Driver as Falsify

import Test.Tasty.Falsify.CLI

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Tasty test options
--
-- Most all of these are command line options, but not all; some are set on a
-- test-by-test basis, such as 'Falsify.ExpectFailure'.
data TestOptions = TestOptions {
      -- | Do we expect this test to fail?
      expectFailure :: Falsify.ExpectFailure

      -- | Override verbose mode for this test
    , overrideVerbose :: Maybe Falsify.Verbose

      -- | Override the maximum number of shrink steps for this test
    , overrideMaxShrinks :: Maybe Word

      -- | Override the number of tests
    , overrideNumTests :: Maybe Word

      -- | Override how many tests can be discarded per successful test
    , overrideMaxRatio :: Maybe Word
    }

instance Default TestOptions where
  def = TestOptions {
        expectFailure      = Falsify.DontExpectFailure
      , overrideVerbose    = Nothing
      , overrideMaxShrinks = Nothing
      , overrideNumTests   = Nothing
      , overrideMaxRatio   = Nothing
      }

{-------------------------------------------------------------------------------
  Construct @falsify@ 'Options'
-------------------------------------------------------------------------------}

assembleFalsifyOptions ::
     Tasty.OptionSet  -- ^ CLI flags
  -> TestOptions      -- ^ User specified test options
  -> (Falsify.Verbose, Falsify.Options)
assembleFalsifyOptions optionSet testOpts = (
      fromMaybe cliVerbosity (overrideVerbose testOpts)
    ,   maybe id
          (\x o -> o{Falsify.maxShrinks = Just x})
          (overrideMaxShrinks testOpts)
      $ maybe id
          (\x o -> o{Falsify.tests = x})
          (overrideNumTests testOpts)
      $ maybe id
          (\x o -> o{Falsify.maxRatio = x})
          (overrideMaxRatio testOpts)
      $ cliOptions
    )
  where
    (cliVerbosity, cliOptions) = extractFalsifyCliFlags optionSet
