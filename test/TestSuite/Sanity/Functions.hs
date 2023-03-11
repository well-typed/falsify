module TestSuite.Sanity.Functions (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Word

import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set

import Test.Falsify.Generator (Gen, Fun, pattern Fn)

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.Range      as Range
import qualified Test.Falsify.SampleTree as SampleTree

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Functions" [
      testGroup "BoolToBool" [
          testCase "notConstant" test_BoolToBool_notConstant
        , testCase "constant"    test_BoolToBool_constant
        ]
    , testGroup "Word8ToBool" [
          testCase "constant" test_Word8ToBool_constant
        ]
    , testGroup "IntegerToBool" [
          testCase "constant" test_IntegerToBool_constant
        ]
    , testGroup "IntToInt" [
          testCase "mapFilter" test_IntToInt_mapFilter
        ]
    , testCase "StringToBool" test_StringToBool
    ]

{-------------------------------------------------------------------------------
  Functions @Bool -> Bool@
-------------------------------------------------------------------------------}

test_BoolToBool_notConstant :: Assertion
test_BoolToBool_notConstant = do
    assertEqual "" expected $
      show . NE.last $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 1)
  where
    expected :: String
    expected = "{_->False}"

    gen :: Gen (Fun Bool Bool)
    gen = Gen.fun (Gen.bool False)

    -- "No function Bool -> Bool can be constant"
    prop :: Fun Bool Bool -> Bool
    prop (Fn f) = f False /= f True

test_BoolToBool_constant :: Assertion
test_BoolToBool_constant = do
    assertEqual "" expected $
      show . NE.last $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 0)
  where
    expected :: String
    expected = "{True->True, _->False}"

    gen :: Gen (Fun Bool Bool)
    gen = Gen.fun (Gen.bool False)

    -- "Every function Bool -> Bool is constant"
    prop :: Fun Bool Bool -> Bool
    prop (Fn f) = f False == f True

{-------------------------------------------------------------------------------
  Functions @Word8 -> Bool@
-------------------------------------------------------------------------------}

test_Word8ToBool_constant :: Assertion
test_Word8ToBool_constant = do
    assertEqual "" expected $
      show . NE.last $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 0)
  where
    expected :: String
    expected = "{254->True, _->False}"

    gen :: Gen (Fun Word8 Bool)
    gen = Gen.fun (Gen.bool False)

    -- "Every function Word8 -> Bool is constant"
    prop :: Fun Word8 Bool -> Bool
    prop (Fn f) =
        (\s -> Set.size s == 1) $
          Set.fromList (map f [minBound .. maxBound])

{-------------------------------------------------------------------------------
  Functions @Integer -> Bool@

  This is the first test where the input domain is infinite.
-------------------------------------------------------------------------------}

test_IntegerToBool_constant :: Assertion
test_IntegerToBool_constant = do
    assertEqual "" expected $
      show . NE.last $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 4)
  where
    expected :: String
    expected = "{1618->True, _->False}"

    gen :: Gen (Fun Integer Bool)
    gen = Gen.fun (Gen.bool False)

    -- "Every fn from Integer to Bool must give the same result for π and φ"
    prop :: Fun Integer Bool -> Bool
    prop (Fn f) = f 3142 == f 1618

{-------------------------------------------------------------------------------
  Functions @Int -> Int@
-------------------------------------------------------------------------------}

test_IntToInt_mapFilter :: Assertion
test_IntToInt_mapFilter = do
    assertEqual "" expected $
      show . NE.last $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 1)
  where
    expected :: String
    expected = "({_->0},{73->True, _->False},[73])"

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
      show . NE.last $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 1)
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

