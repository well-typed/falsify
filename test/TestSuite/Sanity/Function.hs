module TestSuite.Sanity.Function (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify.Debugging
import Test.Falsify.Generator (Gen, Fun, pattern Fn)

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.Range      as Range
import qualified Test.Falsify.SampleTree as SampleTree

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Function" [
      testCase "mapFilter" test_IntToInt_mapFilter
    , testCase "StringToBool" test_StringToBool
    ]

{-------------------------------------------------------------------------------
  Functions @Int -> Int@
-------------------------------------------------------------------------------}

test_IntToInt_mapFilter :: Assertion
test_IntToInt_mapFilter = do
    assertEqual "" expected $
      show . last $ shrink (not . prop) gen (SampleTree.fromSeed 1)
  where
    expected :: String
    expected = "({_->0},{98->True, _->False},[98])"

    gen :: Gen (Fun Int Int, Fun Int Bool, [Int])
    gen =
            (,,)
        <$> Gen.fun
              (Gen.integral $ Range.between (0, 100))
        <*> Gen.fun
              (Gen.bool False)
        <*> Gen.list
              (Range.between (0, 10))
              (Gen.integral $ Range.between (0, 100))

    prop :: (Fun Int Int, Fun Int Bool, [Int]) -> Bool
    prop (Fn f, Fn p, xs) =
        map f (filter p xs) == filter p (map f xs)

{-------------------------------------------------------------------------------
  Functions @String -> Bool@

  This example (as well as 'test_IntToInt_mapFilter') is adapted from
  Koen Claessen's presentation "Shrinking and showing functions"
  at Haskell Symposium 2012 <https://www.youtube.com/watch?v=CH8UQJiv9Q4>.
-------------------------------------------------------------------------------}

test_StringToBool :: Assertion
test_StringToBool = do
    assertEqual "" expected $
      show . last $ shrink (not . prop) gen (SampleTree.fromSeed 9)
  where
    expected :: String
    expected = "{\"Standard ML\"->True, _->False}"

    gen :: Gen (Fun String Bool)
    gen = Gen.fun (Gen.bool False)

    prop :: Fun String Bool -> Bool
    prop (Fn p) = p "Standard ML" `implies` p "Haskell"

    implies :: Bool -> Bool -> Bool
    implies False _ = True
    implies True  b = b
