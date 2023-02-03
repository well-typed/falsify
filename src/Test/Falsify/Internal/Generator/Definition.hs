module Test.Falsify.Internal.Generator.Definition (
    -- * Definition
    Gen(..)
    -- * Primitive generators
  , prim
  , primWith
    -- * Combinators
  , withoutShrinking
    -- * Running
  , run
  , runExplain
  ) where

import Control.Monad
import Data.Word

import Test.Falsify.Internal.Generator.Truncated
import Test.Falsify.Internal.Search
import Test.Falsify.SampleTree (SampleTree)

import qualified Test.Falsify.SampleTree as SampleTree

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Generator of a random value
--
-- Generators can be combined through their 'Functor', 'Applicative' and 'Monad'
-- interfaces. The primitive generator is 'prim', but most users will probably
-- want to construct their generators using the predefined from
-- "Test.Falsify.Generator" as building blocks.
--
-- Generators support \"internal integrated shrinking\". Shrinking is
-- /integrated/ in the sense of Hedgehog, meaning that we don't write a separate
-- shrinker at all, but the shrink behaviour is implied by the generator. For
-- example, if you have a generator @genList@ for a list of numbers, then
--
-- > filter even <$> genList
--
-- will only generate even numbers, and that property is automatically preserved
-- during shrinking. Shrinking is /internal/ in the sense of Hypothesis, meaning
-- that unlike in Hedgehog, shrinking works correctly even in the context of
-- monadic bind. For example, if you do
--
-- > do n <- genListLength
-- >    replicateM n someOtherGen
--
-- then we can shrink @n@ and the results from @someOtherGen@ in any order (that
-- said, users may prefer to use the dedicated
-- 'Test.Falsify.Generator.Compound.list' generator for this purpose, which
-- improves on this in a few ways).
data Gen a where
  Pure :: a -> Gen a
  Prim :: (Word64 -> [Word64]) -> (Word64 -> a) -> Gen a
  Bind :: Gen a -> (a -> Gen b) -> Gen b

{-------------------------------------------------------------------------------
  Primitive generators
-------------------------------------------------------------------------------}

-- | Uniform selection of 'Word64', shrinking towards 0, using binary search
--
-- This is a primitive generator; most users will probably not want to use this
-- generator directly.
prim :: Gen Word64
prim = primWith binarySearch

-- | Generalization of 'prim' that allows to override the shrink behaviour
--
-- This is only required in rare circumstances. Most users will probably never
-- need to use this generator.
primWith :: (Word64 -> [Word64]) -> Gen Word64
primWith shrinker = Prim shrinker id

{-------------------------------------------------------------------------------
  Composition
-------------------------------------------------------------------------------}

instance Functor Gen where
  fmap   = liftM

instance Applicative Gen where
  pure   = Pure
  (<*>)  = ap

instance Monad Gen where
  return = pure
  (>>=)  = Bind

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Disable shrinking in the given generator
--
-- This should be considered a hint only: in particular, values may /always/ be
-- shrunk to their minimum value, and if the structure of a generator changes
-- and random samples are interpreted in a different context, we might "shrink"
-- from any value to another other value (including even larger values).
--
-- This function is only occassionally necessary; most users will probably not
-- need to use it.
withoutShrinking :: Gen a -> Gen a
withoutShrinking = go
  where
    go :: Gen a -> Gen a
    go (Pure x)   = Pure x
    go (Prim _ f) = Prim (const []) f
    go (Bind x f) = Bind (go x) (go . f)

{-------------------------------------------------------------------------------
  Running
-------------------------------------------------------------------------------}

run :: Gen a -> SampleTree -> a
run g = snd . runExplain g

runExplain :: Gen a -> SampleTree -> (Truncated, a)
runExplain = flip go
  where
    go :: SampleTree -> Gen a -> (Truncated, a)
    go st = \case
        Pure x   -> (E, x)
        Prim _ f -> let s = SampleTree.next st
                    in (S s, f s)
        Bind x f -> let (l, r)  = SampleTree.split st
                        (l', a) = go l x
                        (r', b) = go r (f a)
                    in (B l' r', b)