module TestSuite.Sanity.Selective (tests) where

import Control.Selective
import Data.Falsify.List (pairwiseAll, pairwiseAny)
import Data.Falsify.Tree (Tree (..))
import Data.Maybe (mapMaybe)
import Data.Word
import System.Timeout
import Test.Falsify.SampleTree (SampleTree)
import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify.Debugging
import Test.Falsify.Generator (Gen)

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.SampleTree as SampleTree

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Selective" [
      testGroup "either" [
          testCaseInfo "ifM"        test_either_ifM
        , testCaseInfo "ifThenElse" test_either_ifThenElse
        , testCaseInfo "ifS"        test_either_ifS
        ]
    , testGroup "tree" [
          testCaseInfo "ifBoth" test_tree_ifBoth
        , testGroup "ifS" [
              testCase  "10" $ test_tree_ifS  10
            , testCase  "20" $ test_tree_ifS  20
            , testCase  "30" $ test_tree_ifS  30
            , testCase  "40" $ test_tree_ifS  40
            , testCase  "50" $ test_tree_ifS  50
            , testCase  "60" $ test_tree_ifS  60
            , testCase  "70" $ test_tree_ifS  70
            , testCase  "80" $ test_tree_ifS  80
            , testCase  "90" $ test_tree_ifS  90
            , testCase "100" $ test_tree_ifS 100
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Either

  We only only primitive generators here (avoiding generators like
  'Test.Falsify.Reexported.Generator.Simple.bool') to avoid getting distracted
  by specific implementation details of derived generators.
-------------------------------------------------------------------------------}

test_either_ifM :: IO String
test_either_ifM = do
    let results =
          mapMaybe
            (testShrinking gen prop_either $ pairwiseAll sameValue)
            [0 .. 10000]
    case mapMaybe isFailure results of
      []  -> return $ "passed " ++ show (length results) ++ " tests"
      f:_ -> assertFailure $ show f
  where
    -- If we use monadic bind, the seed for the Right value is reused when
    -- when we shrink it to Left: they are not independent.
    --
    -- Here this is still somewhat reasonable, but in general this means we
    -- will reuse a seed reduced in one context in a completely different
    -- context, which may not make any sense at all.
    sameValue ::
         (Word64, Either Word64 Word64)
      -> (Word64, Either Word64 Word64)
      -> Bool
    sameValue (_, Right x) (_, Left y) = x == y
    sameValue _            _           = True

    gen :: Gen (Word64, Either Word64 Word64)
    gen = (,) <$> Gen.prim
              <*> ifM ((== 0) <$> Gen.prim)
                      (Left   <$> Gen.prim)
                      (Right  <$> Gen.prim)

test_either_ifThenElse :: IO String
test_either_ifThenElse = do
    let results =
          mapMaybe
            (testShrinking gen prop_either $ pairwiseAll stayRight)
            [0 .. 10000]
    case mapMaybe isFailure results of
      []  -> return $ "passed " ++ show (length results) ++ " tests"
      f:_ -> assertFailure $ show f
  where
    -- Since we are generating the left value before the right value, if we
    -- /start/ with a right value, we will then shrink the left value first even
    -- though it is not used: indeed, this /must/ always succeed precisely
    -- /because/ that left value is not used. At that point we can no longer
    -- reduce the Right to a Left, because @Left 0@ is not a counterexample.
    stayRight ::
         (Word64, Either Word64 Word64)
      -> (Word64, Either Word64 Word64)
      -> Bool
    stayRight (_, Right _) (_, Left _) = False
    stayRight _            _           = True

    gen :: Gen (Word64, Either Word64 Word64)
    gen = (,) <$> Gen.prim
              <*> ifBoth ((== 0) <$> Gen.prim)
                         (Left   <$> Gen.prim)
                         (Right  <$> Gen.prim)

test_either_ifS :: IO String
test_either_ifS = do
    let results =
          mapMaybe
            (testShrinking gen prop_either $ pairwiseAny independentShrink)
            [0 .. 10000]
    -- Unlike in the tests above, we are checking for /some tests to succeed/,
    -- instead of for /no tests to fail/.
    case filter isOk results of
      [] -> assertFailure "No tests passed"
      ok -> return $ "passed " ++ show (length ok) ++ " tests"
  where
    independentShrink ::
         (Word64, Either Word64 Word64)
      -> (Word64, Either Word64 Word64)
      -> Bool
    independentShrink (_, Right x) (_, Left y) = x /= y
    independentShrink _            _           = False

    gen :: Gen (Word64, Either Word64 Word64)
    gen = (,) <$> Gen.prim
              <*> ifS ((== 0) <$> Gen.prim)
                      (Left   <$> Gen.prim)
                      (Right  <$> Gen.prim)

prop_either :: (Word64, Either Word64 Word64) -> Bool
prop_either (x, Right y) = x < 10 || y > x
prop_either (x, Left  y) = x <  1 || y < x

{-------------------------------------------------------------------------------
  Tree

  In this test we construct a "biased tree" (aka list) using a generator for a
  /complete/ tree but then only using part of the result. Clearly, if we
  /actually/ used the entire complete tree, this would have exponential
  complexity, so that's not an option.

  The problem is not in /generation/, which is sufficiently lazy, but in
  shrinking. With the monadic interface, there are two non-solutions:

  - With the shrinking shortcut in place (reducing entire prats of the tree
    to 'Minimal'), then shrinking isn't all that interesting: the part of the
    tree we're not using will be set to all zeroes immediately (this is what
    the @either@ examples were demonstrating)
  - Without the shrinking shortcut in place, the /generator/ might not look
    at the full complete tree, but the /shrinker/ will, and so shrinking will
    have abysmal performance. This is demonstrated in 'test_tree_ifBoth'.

  With the selective interface, however, everything works just fine.
-------------------------------------------------------------------------------}

test_tree_ifBoth :: IO String
test_tree_ifBoth = do
    let depth = 15
    assertBool "initial" $
      isBiased $
        run (tree ifBoth depth) (sampleTree depth)
    didTimeout <- timeout 10_000_000 $ assertBool "shrunk" $
      all isBiased $
        shrink
          (const True)
          (tree ifBoth depth)
          (sampleTree depth)
    case didTimeout of
      Just () -> assertFailure "Expected timeout, but did not get it"
      Nothing -> return "Timed out as expected"
  where
    -- We reuse the depth as a seed
    sampleTree :: Word64 -> SampleTree
    sampleTree i = SampleTree.map (\s -> 1 + s `mod` 100) $ SampleTree.fromSeed i

test_tree_ifS :: Word64 -> Assertion
test_tree_ifS depth = do
    assertBool "initial" $
      isBiased $
        run (tree ifS depth) (sampleTree depth)
    assertBool "shrunk" $
      all isBiased $
        shrink
          (const True)
          (tree ifS depth)
          (sampleTree depth)
  where
    -- We reuse the depth as a seed
    sampleTree :: Word64 -> SampleTree
    sampleTree i = SampleTree.map (\s -> 1 + s `mod` 10) $ SampleTree.fromSeed i

isBiased :: Tree a -> Bool
isBiased Leaf                         = True
isBiased (Branch _ Leaf     t       ) = isBiased t
isBiased (Branch _ t        Leaf    ) = isBiased t
isBiased (Branch _ Branch{} Branch{}) = False

tree ::
     (forall a. Gen Bool -> Gen a -> Gen a -> Gen a)
  -> Word64 -> Gen (Tree Word64)
tree if_ = go
  where
    go :: Word64 -> Gen (Tree Word64)
    go 0 = pure Leaf
    go d =
        Gen.prim `bindWithoutShortcut` \x ->
        if_ ((== 0) <$> Gen.prim)
            ((\t -> Branch x t Leaf) <$> go (d - 1))
            ((\t -> Branch x Leaf t) <$> go (d - 1))

{-------------------------------------------------------------------------------
  Testing auxiliary
-------------------------------------------------------------------------------}

data TestResult a = TestOk | TestFailed a

isOk :: TestResult a -> Bool
isOk TestOk         = True
isOk (TestFailed _) = False

isFailure :: TestResult a -> Maybe a
isFailure TestOk         = Nothing
isFailure (TestFailed x) = Just x

-- | Test shrinking for a particular PRNG seed
--
-- Returns 'Nothing' if the test must be skipped (because the property we are
-- finding a counter-example to is in fact satisified by the initial value
-- generated by this seed).
testShrinking :: forall a.
     Gen a         -- ^ Generator
  -> (a -> Bool)   -- ^ Property to shrink with respect to
  -> ([a] -> Bool) -- ^ Property that should hold of the shrink history
  -> Word64        -- ^ PRNG seed
  -> Maybe (TestResult [a])
testShrinking gen propGen propHistory i
  | propGen (run gen st) = Nothing
  | propHistory history  = Just TestOk
  | otherwise            = Just $ TestFailed history
  where
    st :: SampleTree
    st = SampleTree.map (`mod` 100) $ SampleTree.fromSeed i

    history :: [a]
    history = shrink (not . propGen) gen st

{-------------------------------------------------------------------------------
  Generic auxiliary
-------------------------------------------------------------------------------}

ifM :: Gen Bool -> Gen a -> Gen a -> Gen a
ifM cond t f = cond `bindWithoutShortcut` \b -> if b then t else f

ifBoth :: Gen Bool -> Gen a -> Gen a -> Gen a
ifBoth cond t f =
    t `bindWithoutShortcut` \x ->
    f `bindWithoutShortcut` \y ->
    cond `bindWithoutShortcut` \b  ->
    return $ if b then x else y