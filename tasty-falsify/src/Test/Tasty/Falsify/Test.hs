-- | Main @tasty@ 'Tasty.IsTest' instance
module Test.Tasty.Falsify.Test (Test(..)) where

import Data.Tagged

import qualified Test.Tasty.Providers as Tasty

import Test.Falsify
import Test.Falsify.Driver

import Test.Tasty.Falsify.CLI
import Test.Tasty.Falsify.Options

{-------------------------------------------------------------------------------
  Definition

  TODO: older versions of tasty explicitly say to ignore the @reportProgress@
  argument, but this is no longer true as of tasty 1.5.
  <https://github.com/well-typed/falsify/issues/63>
-------------------------------------------------------------------------------}

data Test = Test TestOptions (Property' String ())

instance Tasty.IsTest Test where
  run optionSet (Test testOpts prop) _reportProgress =
      toTastyResult . renderTestOutcome verbose (expectFailure testOpts) <$>
        falsify options prop
    where
      (verbose, options) = assembleFalsifyOptions optionSet testOpts

  testOptions = Tagged allFalsifyCliFlags

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

toTastyResult :: RenderedTestOutcome -> Tasty.Result
toTastyResult RenderedTestOutcome{testPassed, testOutput}
  | testPassed = Tasty.testPassed testOutput
  | otherwise  = Tasty.testFailed testOutput
