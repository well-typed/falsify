module Test.Falsify.Internal.Generator.Definition (
    -- * Definition
    Gen(..)
    -- * Primitive generators
  , Prim(..)
  , prim
  , primWith
    -- * Combinators
  , withoutShrinking
    -- * Running
  , run
  , runExplain
  ) where

import Control.Monad
import Control.Selective
import Data.Bifunctor
import Data.Word

import Test.Falsify.Internal.Generator.Truncated
import Test.Falsify.Internal.Search
import Test.Falsify.SampleTree (SampleTree, Sample, sampleValue)

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
  Pure   :: a -> Gen a
  Prim   :: Prim a -> Gen a
  Bind   :: Gen a -> (a -> Gen b) -> Gen b
  Select :: Gen (Either a b) -> Gen (a -> b) -> Gen b

{-------------------------------------------------------------------------------
  Primitive generators
-------------------------------------------------------------------------------}

-- | Primitive generator
--
-- This is an internal type, not part of the public API.
--
-- It is important that 'Prim' supports a 'Functor' instance. If a primitive
-- generator would always return something of type 'Word64', then we would need
-- rely on 'Bind' to implement 'Functor' for 'Gen', which would have unfortunate
-- consequences. For example, it would mean that a generator such as
--
-- > do x <- bool
-- >    if x then negate <$> prim else prim
--
-- would shrink in unexpected ways: @negate <$> prim@ and @prim@ would look at
-- different parts of the sample tree.
data Prim a = P (Sample -> [Word64]) (Sample -> a)
  deriving (Functor)

-- | Uniform selection of 'Word64', shrinking towards 0, using binary search
--
-- This is a primitive generator; most users will probably not want to use this
-- generator directly.
prim :: Gen Word64
prim = sampleValue <$> primWith (binarySearch . sampleValue)

-- | Generalization of 'prim' that allows to override the shrink behaviour
--
-- This is only required in rare circumstances. Most users will probably never
-- need to use this generator.
primWith :: (Sample -> [Word64]) -> Gen Sample
primWith shrinker = Prim (P shrinker id)

{-------------------------------------------------------------------------------
  Composition
-------------------------------------------------------------------------------}

instance Functor Gen where
  fmap g (Pure x)     = Pure (g x)
  fmap g (Prim p)     = Prim (fmap g p)
  fmap g (Bind x f)   = Bind x (fmap g . f)
  fmap g (Select e f) = Select (fmap (second g) e) (fmap (g .) f)

instance Applicative Gen where
  pure   = Pure
  (<*>)  = ap

instance Selective Gen where
  select = Select

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
    go (Pure x)       = Pure x
    go (Prim (P _ f)) = Prim (P (const []) f)
    go (Bind x f)     = Bind (go x) (go . f)
    go (Select e f)   = Select (go e) (go f)

{-------------------------------------------------------------------------------
  Running
-------------------------------------------------------------------------------}

run :: Gen a -> SampleTree -> a
run g = snd . runExplain g

runExplain :: Gen a -> SampleTree -> (Truncated, a)
runExplain = flip go
  where
    go :: forall a. SampleTree -> Gen a -> (Truncated, a)
    go st = \case
        Pure x       -> (E, x)
        Prim (P _ f) -> let s = SampleTree.next st in (S s, f s)
        Bind x f     -> let (l, r)  = SampleTree.split st
                            (l', a) = go l x
                            (r', b) = go r (f a)
                        in (B l' r', b)
        Select e f   -> let (l, r)   = SampleTree.split st
                            (l', ab) = go l e
                        in case ab of
                             Left  a -> let (r', b) = ($ a) <$> go r f in
                                        (B l' r', b)
                             Right b -> (B l' E,  b)
