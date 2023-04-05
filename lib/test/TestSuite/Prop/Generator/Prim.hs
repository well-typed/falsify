module TestSuite.Prop.Generator.Prim (tests) where

import Prelude hiding (pred)

import Control.Monad
import Control.Selective
import Data.Default
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

import TestSuite.Util.List

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Prim" [
    testGroup "prim" [
        testProperty "shrinking" prop_prim_shrinking
      , testGroup "minimum" [
            testProperty (show target) $ prop_prim_minimum target
          | target <- [0 .. 4]
          ]
      , testPropertyWith (def { expectFailure = ExpectFailure })
          "prim_minimum_wrong" prop_prim_minimum_wrong
      ]
    , testGroup "applicative" [
          testGroup "pair" [
              testProperty "shrinking" prop_applicative_pair_shrinking
            , testProperty "minimum1"  prop_applicative_pair_minimum1
            , testProperty "minimum2"  prop_applicative_pair_minimum2
            ]
        , testGroup "replicateM" [
              testProperty "shrinking" prop_applicative_replicateM_shrinking
            , testProperty "minimum"   prop_applicative_replicateM_minimum
            ]
        ]
    , testGroup "monad" [
          testGroup "maybe" [
              testGroup "towardsNothing" [
                  testProperty "shrinking" prop_monad_maybe_towardsNothing_shrinking
                , testProperty "minimum"   prop_monad_maybe_towardsNothing_minimum
                , testPropertyWith expectFailure
                     "shrinking_wrong" prop_monad_maybe_towardsNothing_shrinking_wrong
                ]
            , testGroup "towardsJust" [
                  testProperty "shrinking" prop_monad_maybe_towardsJust_shrinking
                , testProperty "minimum"   prop_monad_maybe_towardsJust_minimum
                , testPropertyWith expectFailure
                     "minimum_wrong" prop_monad_maybe_towardsJust_minimum_wrong
                ]
            ]
        , testGroup "either" [
              testProperty "shrinking" prop_monad_either_shrinking
            ]
        ]
    , testGroup "selective" [
          testGroup "either" [
              testPropertyWith expectFailure
                "shrinking" prop_selective_either_shrinking_wrong
            ]
        ]
    , testGroup "captureLocalTree" [
          testProperty "shrinking1" prop_captureLocalTree_shrinking1
        , testProperty "shrinking2" prop_captureLocalTree_shrinking2
        ]
    , testGroup "stream" [
          testProperty "shrinking1" prop_stream_shrinking1
        , testProperty "shrinking2" prop_stream_shrinking2
        , testProperty "minimum"    prop_stream_minimum
        ]
    ]
  where
    expectFailure :: TestOptions
    expectFailure = def {
        expectFailure    = ExpectFailure
      , overrideNumTests = Just 100_000
      }

{-------------------------------------------------------------------------------
  Prim
-------------------------------------------------------------------------------}

-- Gen.prime is the only generator where we a /strict/ inequality
prop_prim_shrinking :: Property ()
prop_prim_shrinking = testShrinkingOfGen P.gt $ Gen.prim

-- The minimum is always 0, unless 0 is not a counter-example
prop_prim_minimum :: Word64 -> Property ()
prop_prim_minimum target = do
    testMinimum (P.expect $ if target == 0 then 1 else 0) $ do
      x <- gen $ Gen.prim
      unless (x == target) $ testFailed x

-- | Just to verify that we if we specify the /wrong/ minimum, we get a failure
prop_prim_minimum_wrong :: Property ()
prop_prim_minimum_wrong =
    testMinimum (P.expect 1) $ do
      x <- gen $ Gen.prim
      testFailed x

{-------------------------------------------------------------------------------
  Applicative: pairs
-------------------------------------------------------------------------------}

prop_applicative_pair_shrinking :: Property ()
prop_applicative_pair_shrinking =
    testShrinkingOfGen (P.relatedBy ("validShrink", validShrink)) $
      (,) <$> Gen.prim <*> Gen.prim
  where
    validShrink :: (Word64, Word64) -> (Word64, Word64) -> Bool
    validShrink (x, y) (x', y') = x >= x' && y >= y'

prop_applicative_pair_minimum1 :: Property ()
prop_applicative_pair_minimum1 =
    testMinimum (P.expect (1, 0)) $ do
      (x, y) <- gen $ (,) <$> Gen.prim <*> Gen.prim
      unless (x == 0 || x < y) $ testFailed (x, y)

prop_applicative_pair_minimum2 :: Property ()
prop_applicative_pair_minimum2 =
    testMinimum (P.expect (1, 1)) $ do
      (x, y) <- gen $ (,) <$> Gen.prim <*> Gen.prim
      unless (x == 0 || x > y) $ testFailed (x, y)

{-------------------------------------------------------------------------------
  Applicative: replicateM
-------------------------------------------------------------------------------}

genList :: Gen [Word64]
genList = do
    n <- (`min` 10) <$> Gen.prim
    replicateM (fromIntegral n) Gen.prim

prop_applicative_replicateM_shrinking :: Property ()
prop_applicative_replicateM_shrinking =
    testShrinkingOfGen (P.relatedBy ("validShrink", validShrink)) genList
  where
    validShrink :: [Word64] -> [Word64] -> Bool
    validShrink []      []    = True
    validShrink []      (_:_) = False
    validShrink (_:_)   []    = True
    validShrink (x:xs) (y:ys) = x >= y && validShrink xs ys

prop_applicative_replicateM_minimum :: Property ()
prop_applicative_replicateM_minimum =
    testMinimum (P.expect [0,1]) $ do
      xs <- gen $ genList
      unless (pairwiseAll (==) xs) $ testFailed xs

{-------------------------------------------------------------------------------
  Monad: Maybe (towards 'Nothing')
-------------------------------------------------------------------------------}

genSmall :: Gen Word64
genSmall = do
    startWithEven <- Gen.prim
    if startWithEven >= maxBound `div` 2
      then Gen.exhaustive 100
      else Gen.exhaustive  99 -- smaller bound, to ensure shrinking

genTowardsNothing :: Gen (Maybe Word64, Word64)
genTowardsNothing = do
    genNothing <- (== 0) <$> Gen.prim
    if genNothing
      then (\  y -> (Nothing, y)) <$>              genSmall
      else (\x y -> (Just x,  y)) <$> genSmall <*> genSmall

prop_monad_maybe_towardsNothing_shrinking :: Property ()
prop_monad_maybe_towardsNothing_shrinking =
    testShrinkingOfGen
      (P.relatedBy ("validShrink", validShrink))
      genTowardsNothing
  where
    validShrink :: (Maybe Word64, Word64) -> (Maybe Word64, Word64) -> Bool
    validShrink (Nothing , y) (Nothing , y') = y >= y'
    validShrink (Just _  , _) (Nothing , _ ) = True -- See @.._wrong@ property
    validShrink (Nothing , _) (Just _  , _ ) = False
    validShrink (Just x  , y) (Just x' , y') = x >= x' && y >= y'

prop_monad_maybe_towardsNothing_minimum :: Property ()
prop_monad_maybe_towardsNothing_minimum =
    testMinimum (P.expect expected) $ do
      (x, y) <- gen $ genTowardsNothing
      unless (even y) $ testFailed (x, y)
  where
    -- We are using different generators, a switch from 'Just' to 'Nothing'
    -- might temporarily because @y@ to increase (see @.._wrong@), but we will
    -- then continue to shrink that value.
    expected :: (Maybe Word64, Word64)
    expected = (Nothing, 1)

prop_monad_maybe_towardsNothing_shrinking_wrong :: Property ()
prop_monad_maybe_towardsNothing_shrinking_wrong =
    testShrinkingOfGen
      (P.relatedBy ("validShrink", validShrink))
      genTowardsNothing
  where
    -- This property is wrong: the two generators on the RHS have a different
    -- structure, and therefore shrink independently. When we switch the
    -- LHS from Just to Nothing, we run a /different/ generator.
    validShrink :: (Maybe Word64, Word64) -> (Maybe Word64, Word64) -> Bool
    validShrink (Nothing , y) (Nothing , y') = y >= y'
    validShrink (Just _  , y) (Nothing , y') = y >= y'
    validShrink (Nothing , _) (Just _  ,  _) = False
    validShrink (Just x  , y) (Just x' , y') = x >= x' && y >= y'

{-------------------------------------------------------------------------------
  Monad: Maybe (towards 'Just')

  Unlike hypothesis, we are always dealing with infinite sample tree; if a
  "simpler" test case needs more samples, then they are available.
-------------------------------------------------------------------------------}

genTowardsJust :: Gen (Maybe Word64, Word64)
genTowardsJust = do
    genJust <- (== 0) <$> Gen.prim
    if genJust
      then (\x y -> (Just x,  y)) <$> genSmall <*> genSmall
      else (\  y -> (Nothing, y)) <$>              genSmall

prop_monad_maybe_towardsJust_shrinking :: Property ()
prop_monad_maybe_towardsJust_shrinking =
    testShrinkingOfGen
      (P.relatedBy ("validShrink", validShrink))
      genTowardsJust
  where
    validShrink :: (Maybe Word64, Word64) -> (Maybe Word64, Word64) -> Bool
    validShrink (Nothing , y) (Nothing , y') = y >= y'
    validShrink (Just _  , _) (Nothing , _ ) = False
    validShrink (Nothing , _) (Just _  , _ ) = True
    validShrink (Just x  , y) (Just x' , y') = x >= x' && y >= y'

prop_monad_maybe_towardsJust_minimum :: Property ()
prop_monad_maybe_towardsJust_minimum =
    testMinimum (P.satisfies ("expected", expected)) $ do
      (x, y) <- gen $ genTowardsJust
      unless (even y) $ testFailed (x, y)
  where
    expected :: (Maybe Word64, Word64) -> Bool
    expected (Just _  , y) = y == 1
    expected (Nothing , _) = True

prop_monad_maybe_towardsJust_minimum_wrong :: Property ()
prop_monad_maybe_towardsJust_minimum_wrong =
    testMinimum (P.expect expected) $ do
      (x, y) <- gen $ genTowardsJust
      unless (even y) $ testFailed (x, y)
  where
    -- We might not always be able to shrink from 'Nothing' to 'Just', because
    -- the /value/ of that 'Just' might not be a counter-example; we would need
    -- to take two shrink steps at once (switch from 'Just' to 'Nothing' /and/
    -- reduce the value of the 'Just').
    --
    -- 'Selective' does not help either (it also would need to take two steps);
    -- we /could/ try to solve the problem by generating /both/ values always,
    -- and using only one, but as we know, that is not an effective strategy:
    -- generated-by-not-used values will always be shrunk to their minimal
    -- value, independent of the property.
    expected :: (Maybe Word64, Word64)
    expected = (Just 0, 1)

{-------------------------------------------------------------------------------
  Monad: Either
-------------------------------------------------------------------------------}

genMonadEither :: Gen (Either Word64 Word64)
genMonadEither = do
    genLeft <- (== 0) <$> Gen.prim -- shrink towards left
    if genLeft
      then Left  <$> Gen.prim
      else Right <$> Gen.prim

prop_monad_either_shrinking :: Property ()
prop_monad_either_shrinking =
    testShrinkingOfGen
      (P.relatedBy ("validShrink", validShrink))
      genMonadEither
  where
    -- The 'Left' and 'Right' case use the /same/ part of the sample tree, so
    -- that if we shrink from one to the other, we /must/ get the same value.
    validShrink :: Either Word64 Word64 -> Either Word64 Word64 -> Bool
    validShrink _         (Left 0)   = True -- We can always shrink to 'Minimal'
    validShrink (Left x)  (Left x')  = x >= x'
    validShrink (Left _)  (Right _)  = False
    validShrink (Right x) (Left x')  = x == x'
    validShrink (Right x) (Right x') = x >= x'

{-------------------------------------------------------------------------------
  Selective: either
-------------------------------------------------------------------------------}

genSelectiveEither :: Gen (Either Word64 Word64)
genSelectiveEither =
    ifS ((== 0) <$> Gen.prim)
        (Left  <$> Gen.prim)
        (Right <$> Gen.prim)

prop_selective_either_shrinking_wrong :: Property ()
prop_selective_either_shrinking_wrong =
    testShrinkingOfGen
      (P.relatedBy ("validShrink", validShrink))
      genSelectiveEither
  where
    -- Like in 'prop_monad_either_shrinking', here the two generators are
    -- independent, and so it's entirely possible we might shrink from @Right x@
    -- to @Left y@ for @x /= y@.
    validShrink :: Either Word64 Word64 -> Either Word64 Word64 -> Bool
    validShrink _         (Left 0)   = True -- We can always shrink to 'Minimal'
    validShrink (Left x)  (Left x')  = x >= x'
    validShrink (Left _)  (Right _)  = False
    validShrink (Right x) (Left x')  = x == x'
    validShrink (Right x) (Right x') = x >= x'

{-------------------------------------------------------------------------------
  captureLocalTree
-------------------------------------------------------------------------------}

prop_captureLocalTree_shrinking1 :: Property ()
prop_captureLocalTree_shrinking1 =
    testShrinkingOfGen P.alwaysFail $
      Gen.captureLocalTree

-- Check that we /still/ cannot shrink (i.e., monadic bind is not
-- introducing a bug somewhere)
prop_captureLocalTree_shrinking2 :: Property ()
prop_captureLocalTree_shrinking2 =
    testShrinkingOfGen P.alwaysFail $ do
      t1 <- Gen.captureLocalTree
      t2 <- Gen.captureLocalTree
      return (t1, t2)

{-------------------------------------------------------------------------------
  Stream

  The purpose of this test is to test generation (and shrinking) of infinite
  data structures. The function generation tests will verify that also, but they
  are much more complicated.
-------------------------------------------------------------------------------}

-- | Infinite stream of values
--
-- Intentionally does not have a 'Show' instance!
data Stream a = Stream a (Stream a)

prefix :: Stream a -> Word64 -> [a]
prefix _             0 = []
prefix (Stream x xs) n = x : prefix xs (n - 1)

genStream :: Gen (Stream Word64)
genStream = Stream <$> Gen.exhaustive 10 <*> genStream

genStreamPrefix :: Gen [Word64]
genStreamPrefix = prefix <$> genStream <*> Gen.exhaustive 10

-- | Check that we can test shrinking of infinite structures /at all/
prop_stream_shrinking1 :: Property ()
prop_stream_shrinking1 =
    testShrinkingOfGen P.alwaysPass $
      genStreamPrefix

-- | Check that we shrink in the way we expect
prop_stream_shrinking2 :: Property ()
prop_stream_shrinking2 =
    testShrinkingOfGen pred $
      genStreamPrefix
  where
    pred :: P.Predicate '[[Word64], [Word64]]
    pred = mconcat [
        P.ge `P.on` P.fn ("length", length)
      , P.relatedBy ("elemsRelated", elemsRelated)
      ]

    elemsRelated :: [Word64] -> [Word64] -> Bool
    elemsRelated orig shrunk = and $ zipWith (>=) orig shrunk

prop_stream_minimum :: Property ()
prop_stream_minimum =
    testMinimum (P.expect [0, 0]) $ do
      xs <- gen genStreamPrefix
      unless (pairwiseAll (<) xs) $ testFailed xs

