-- | Utilities for interaction with falsify in ghci
module Test.Falsify.Interactive (
    sample
  ) where

import System.Random.SplitMix

import Test.Falsify.Internal.Generator

import qualified Test.Falsify.SampleTree as SampleTree

-- | Sample generator
sample :: Gen a -> IO a
sample gen = do
    prng <- initSMGen
    let (x, _shrunk) = runGen gen (SampleTree.fromPRNG prng)
    return x
