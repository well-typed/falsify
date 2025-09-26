module Test.Falsify.Internal.Generator.Definition (
    -- * Definition
    Gen(..)
  , bindWithoutShortcut
  , minimalValue
    -- * Primitive generators
  , prim
  , primWith
  , exhaustive
  , captureLocalTree
    -- * Generator independence
  , bindIntegral
  , perturb
    -- * Combinators
  , withoutShrinking
  ) where

import Control.Monad
import Control.Selective
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Word
import Optics.Core (Lens', (%))

import qualified Optics.Core as Optics

import Data.Falsify.Integer (Bit(..), encIntegerEliasG)
import Test.Falsify.Internal.SampleTree (SampleTree(..), Sample (..), pattern Inf)
import Test.Falsify.Internal.Search

import qualified Test.Falsify.Internal.SampleTree as SampleTree

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
--
-- NOTE: 'Gen' is /NOT/ an instance of 'Alternative'; this would not be
-- compatible with the generation of infinite data structures. For the same
-- reason, we do not have a monad transformer version of Gen either.
newtype Gen a = Gen { runGen :: SampleTree -> (a, [SampleTree]) }
  deriving stock (Functor)

instance Applicative Gen where
  pure x = Gen $ \_st -> (x, [])
  (<*>)  = ap

instance Monad Gen where
  return  = pure
  x >>= f = Gen $ \(Inf s l r) ->
      let (a, ls) = runGen x l
          (b, rs) = runGen (f a) r
      in (b, combineShrunk s (l :| ls) (r :| rs))

instance Selective Gen where
  select e f = Gen $ \(Inf s l r) -> do
      let (ma, ls) = runGen e l
      case ma of
        Left a ->
          let (f', rs) = runGen f r
          in (f' a, combineShrunk s (l :| ls) (r :| rs))
        Right b ->
          (b, combineShrunk s (l :| ls) (r :| []))

-- | Combine shrunk left and right sample trees
--
-- This is an internal function only.
combineShrunk ::
     Sample
  -> NonEmpty SampleTree -- ^ Original and shrunk left  trees
  -> NonEmpty SampleTree -- ^ Original and shrunk right trees
  -> [SampleTree]
combineShrunk s (l :| ls) (r :| rs) = shortcut $ concat [
      [SampleTree s l' r  | l' <- unlessMinimal l ls]
    , [SampleTree s l  r' | r' <- unlessMinimal r rs]
    ]
  where
    -- We must be careful not to force @ls@/@rs@ if the tree is already minimal.
    unlessMinimal :: SampleTree -> [a] -> [a]
    unlessMinimal Minimal _  = []
    unlessMinimal _       xs = xs

    shortcut :: [SampleTree] -> [SampleTree]
    shortcut [] = []
    shortcut ts = Minimal : ts

-- | Varation on @(>>=)@ that doesn't apply the shortcut to 'Minimal'
--
-- This function is primarily useful for debugging @falsify@ itself; users
-- will probably never need it.
bindWithoutShortcut :: Gen a -> (a -> Gen b) -> Gen b
bindWithoutShortcut x f = Gen $ \(Inf s l r) ->
    let (a, ls) = runGen x l
        (b, rs) = runGen (f a) r
    in (b, combine s (l :| ls) (r :| rs))
  where
    -- Variation on 'combineShrunk' that doesn't apply the shortcut
    combine ::
         Sample
      -> NonEmpty SampleTree -- ^ Original and shrunk left  trees
      -> NonEmpty SampleTree -- ^ Original and shrunk right trees
      -> [SampleTree]
    combine s (l :| ls) (r :| rs) = concat [
          [SampleTree s l' r  | l' <- ls]
        , [SampleTree s l  r' | r' <- rs]
        ]

-- | Get the value produced by the generator on the minimal sample tree.
--
-- Having `Gen a` is a proof that `a` is inhabited, so this function
-- gives access to a witness.
minimalValue :: Gen a -> a
minimalValue g = fst (runGen g Minimal)

{-------------------------------------------------------------------------------
  Generator independence
-------------------------------------------------------------------------------}

-- | Selective bind
--
-- Unlike monadic bind, the RHS is generated and shrunk completely independently
-- for each different value of @a@ produced by the LHS.
--
-- This is a generalization of 'bindS' to arbitrary integral values; it is also
-- much more efficient than 'bindS'.
--
-- NOTE: This is only one way to make a generator independent. See 'perturb'
-- for more primitive combinator.
bindIntegral :: Integral a => Gen a -> (a -> Gen b) -> Gen b
bindIntegral x f = x >>= \a -> perturb a (f a)

-- | Run generator on different part of the sample tree depending on @a@
perturb :: Integral a => a -> Gen b -> Gen b
perturb a g = Gen $ \st ->
    let (b, shrunk) = runGen g (Optics.view lens st)
    in (b, map (\st' -> Optics.set lens st' st) shrunk)
  where
    lens :: Lens' SampleTree SampleTree
    lens = computeLens (encIntegerEliasG $ fromIntegral a)

    computeLens :: [Bit] -> Lens' SampleTree SampleTree
    computeLens []       = Optics.castOptic Optics.simple
    computeLens (O : bs) = SampleTree.left  % computeLens bs
    computeLens (I : bs) = SampleTree.right % computeLens bs

{-------------------------------------------------------------------------------
  Primitive generators
-------------------------------------------------------------------------------}

-- | Uniform selection of 'Word64', shrinking towards 0, using binary search
--
-- This is a primitive generator; most users will probably not want to use this
-- generator directly.
prim :: Gen Word64
prim =
    SampleTree.sampleValue <$>
      primWith (binarySearch . SampleTree.sampleValue)

-- | Generalization of 'prim' that allows to override the shrink behaviour
--
-- This is only required in rare circumstances. Most users will probably never
-- need to use this generator.
primWith :: (Sample -> [Word64]) -> Gen Sample
primWith f = Gen $ \(Inf s l r) -> (
      s
    , (\s' -> SampleTree (Shrunk s') l r) <$> f s
    )

-- | Generate arbitrary value @x <= n@
--
-- Unlike 'prim', 'exhaustive' does not execute binary search. Instead, /all/
-- smaller values are considered. This is potentially very expensive; the
-- primary use case for this generator is testing shrinking behaviour, where
-- binary search can lead to some unpredicatable results.
--
-- This does /NOT/ do uniform selection: for small @n@, the generator will with
-- overwhelming probability produce @n@ itself as initial value.
--
-- This is a primitive generator; most users will probably not want to use this
-- generator directly.
exhaustive :: Word64 -> Gen Word64
exhaustive n =
    min n . SampleTree.sampleValue <$>
      primWith (completeSearch . SampleTree.sampleValue)
  where
    completeSearch :: Word64 -> [Word64]
    completeSearch 0 = []
    completeSearch x = takeWhile (<= n) [0 .. pred x]

-- | Capture the local sample tree
--
-- This generator does not shrink.
captureLocalTree :: Gen SampleTree
captureLocalTree = Gen $ \st -> (st, [])

{-------------------------------------------------------------------------------
  Shrinking combinators
-------------------------------------------------------------------------------}

-- | Disable shrinking in the given generator
--
-- Due to the nature of internal shrinking, it is always possible that a
-- generator gets reapplied to samples that were shrunk wrt to a /different/
-- generator. In this sense, 'withoutShrinking' should be considered to be a
-- hint only.
--
-- This function is only occassionally necessary; most users will probably not
-- need to use it.
withoutShrinking :: Gen a -> Gen a
withoutShrinking (Gen g) = Gen $ aux . g
  where
    aux :: (a, [SampleTree]) -> (a, [SampleTree])
    aux (outcome, _) = (outcome, [])
