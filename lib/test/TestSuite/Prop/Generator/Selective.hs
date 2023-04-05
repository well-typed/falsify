module TestSuite.Prop.Generator.Selective (tests) where

import Control.Monad
import Control.Selective
import Data.Default
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Selective" [
      testGroup "pair" [
          testProperty                   "ifM"        $ prop_pair ifM
        , testPropertyWith expectFailure "ifS"        $ prop_pair ifS
        , testProperty                   "ifThenElse" $ prop_pair_ifThenElse
        ]
    ]
  where
    expectFailure :: TestOptions
    expectFailure = def {
          expectFailure    = ExpectFailure
        , overrideNumTests = Just 10_000
        }

{-------------------------------------------------------------------------------
  Either

  We only only primitive generators here (avoiding generators like
  'Test.Falsify.Reexported.Generator.Simple.bool') to avoid getting distracted
  by specific implementation details of derived generators.
-------------------------------------------------------------------------------}

-- If we use monadic bind, the seed for the Right value is reused when
-- when we shrink it to Left: they are not independent.
--
-- Here this is still somewhat reasonable, but in general this means we
-- will reuse a seed reduced in one context in a completely different
-- context, which may not make any sense at all.
propEither ::
     (Word64, Either Word64 Word64)
  -> (Word64, Either Word64 Word64)
  -> Bool
propEither _            (_, Left 0) = True -- Can always shrink to 'Minimal'
propEither (_, Right x) (_, Left y) = x == y
propEither _            _           = True

genPair ::
    (forall a. Gen Bool -> Gen a -> Gen a -> Gen a)
  -> Gen (Word64, Either Word64 Word64)
genPair if_ =
    (,) <$> Gen.prim
        <*> if_ ((== 0) <$> Gen.prim)
                (Left   <$> Gen.exhaustive 100)
                (Right  <$> Gen.exhaustive 100)

prop_pair :: (forall a. Gen Bool -> Gen a -> Gen a -> Gen a) -> Property ()
prop_pair if_ =
    testShrinkingOfGen (P.relatedBy ("propEither", propEither)) $
      genPair if_

prop_pair_ifThenElse :: Property ()
prop_pair_ifThenElse =
    testShrinking (P.relatedBy ("stayRight", stayRight)) $ do
      pair <- gen $ genPair ifBoth
      when (prop pair) $ testFailed pair
  where
    prop :: (Word64, Either Word64 Word64) -> Bool
    prop (x, Right y) = x < 10 || y > x
    prop (x, Left  y) = x <  1 || y < x

    -- Since we are generating the left value before the right value, if we
    -- /start/ with a right value, we will then shrink the left value first even
    -- though it is not used: indeed, this /must/ always succeed precisely
    -- /because/ that left value is not used. At that point we can no longer
    -- reduce the Right to a Left, because @Left 0@ is not a counterexample.
    stayRight ::
         (Word64, Either Word64 Word64)
      -> (Word64, Either Word64 Word64)
      -> Bool
    stayRight _            (_, Left 0) = True -- Can always shrink to 'Minimal'
    stayRight (_, Right _) (_, Left _) = False
    stayRight _            _           = True

{-------------------------------------------------------------------------------
  Generic auxiliary
-------------------------------------------------------------------------------}

ifM :: Gen Bool -> Gen a -> Gen a -> Gen a
ifM cond t f = cond `Gen.bindWithoutShortcut` \b -> if b then t else f

ifBoth :: Gen Bool -> Gen a -> Gen a -> Gen a
ifBoth cond t f =
    t `Gen.bindWithoutShortcut` \x ->
    f `Gen.bindWithoutShortcut` \y ->
    cond `Gen.bindWithoutShortcut` \b  ->
    return $ if b then x else y