module TestSuite.Sanity.Prim (tests) where

import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Data.Falsify.List (pairwiseAll, pairwiseAny)
import Test.Falsify.Debugging
import Test.Falsify.Generator (Gen)
import Test.Falsify.SampleTree (SampleTree)

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.SampleTree as SampleTree

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Prim" [
      testCase "stream"               test_stream
    , testCase "captureLocalTree"     test_captureLocalTree
    , testCase "captureLocalTreeBind" test_captureLocalTreeBind
    ]

{-------------------------------------------------------------------------------
  Infinite streams
-------------------------------------------------------------------------------}

data Stream a = Stream a (Stream a)

prefix :: Stream a -> Word64 -> [a]
prefix _             0 = []
prefix (Stream x xs) n = x : prefix xs (pred n)

test_stream :: Assertion
test_stream = do
    assertEqual "run" [3,7,5,1,3,2,1] $
      uncurry prefix $ run gen $ SampleTree.mod 10 (SampleTree.fromSeed 1)

    let expectedHistory = [
            [3,7,5,1,3,2,1]
          , [2,7,5,1,3,2,1]
          , [1,7,5,1,3,2,1]
          , [1,4,5,1,3,2,1]
          , [1,2,5,1,3,2,1]
          , [1,2,3,1,3,2,1]
          , [1,2,3,1,3,2,1]
          , [1,2,3,1]
          , [1,2,3,1]
          ]
    assertEqual "shrink" expectedHistory $
      fmap (uncurry prefix) $
        shrink (not . prop) gen (SampleTree.mod 10 $ SampleTree.fromSeed 1)
  where
    gen :: Gen (Stream Word64, Word64)
    gen = (,) <$> genStream <*> Gen.prim

    genStream :: Gen (Stream Word64)
    genStream = Stream <$> Gen.prim <*> genStream

    prop :: (Stream Word64, Word64) -> Bool
    prop = aux . uncurry prefix
      where
        aux :: [Word64] -> Bool
        aux xs = pairwiseAll (\x y -> y == succ x) xs
              || pairwiseAny (==)                  xs
              || any (== 0)                        xs

{-------------------------------------------------------------------------------
  captureLocalTree
-------------------------------------------------------------------------------}

test_captureLocalTree :: Assertion
test_captureLocalTree =
    assertEqual "cannot shrink" 1 $
      length $ shrink (const True) gen (SampleTree.fromSeed 0)
  where
    gen :: Gen SampleTree
    gen = Gen.captureLocalTree

test_captureLocalTreeBind :: Assertion
test_captureLocalTreeBind =
    assertEqual "cannot shrink" 1 $
      length $ shrink (const True) gen (SampleTree.fromSeed 0)
  where
    gen :: Gen (SampleTree, SampleTree)
    gen = do
        t1 <- Gen.captureLocalTree
        t2 <- Gen.captureLocalTree
        return (t1, t2)