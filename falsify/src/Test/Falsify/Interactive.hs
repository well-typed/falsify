-- | Utilities for interaction with falsify in ghci
module Test.Falsify.Interactive (
    falsify
  , falsify'
  , sample
  , shrink
  , shrink'
  ) where

import Data.Default
import Data.List.NonEmpty (NonEmpty(..))
import System.Random.SplitMix

import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Property

import qualified Test.Falsify.Driver     as Driver
import qualified Test.Falsify.SampleTree as SampleTree

-- | Sample generator
sample :: Gen a -> IO a
sample g = do
    prng <- initSMGen
    let (x, _shrunk) = runGen g (SampleTree.fromPRNG prng)
    return x

-- | Shrink counter-example
--
-- This will run the generator repeatedly until it finds a counter-example to
-- the given property, and will then shrink it.
--
-- Returns 'Nothing' if no counter-example could be found.
shrink :: forall a. (a -> Bool) -> Gen a -> IO (Maybe a)
shrink p g = falsify $ testGen' (\x -> aux x $ p x) g
  where
    aux :: a -> Bool -> Either a ()
    aux _ True  = Right ()
    aux x False = Left x

-- | Generalization of 'shrink'. Returns the full shrink history.
shrink' :: forall e a. (a -> Maybe e) -> Gen a -> IO (Maybe (NonEmpty e))
shrink' p g = falsify' $ testGen' (aux . p) g
  where
    aux :: Maybe e -> Either e ()
    aux Nothing  = Right ()
    aux (Just x) = Left x

-- | Try to falsify the given property
--
-- Reports the counter-example, if we find any.
falsify :: forall e a. Property' e a -> IO (Maybe e)
falsify = fmap (fmap snd . Driver.failure) . Driver.falsify def

-- | Generalization of 'falsify' that reports the full shrink history
falsify' :: forall e a. Property' e a -> IO (Maybe (NonEmpty e))
falsify' = fmap (fmap snd . Driver.failure') . Driver.falsify def
