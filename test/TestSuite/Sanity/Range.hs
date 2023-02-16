module TestSuite.Sanity.Range (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify.Nudge
import Test.Falsify.Range

import qualified Test.Falsify.Range as Range

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Range" [
      testCase "num" test_num
    ]

test_num :: Assertion
test_num = do
    assertBool "Range-060" $ Range.same expected060 $ Range.num (0, 6) 0
    assertBool "Range-066" $ Range.same expected066 $ Range.num (0, 6) 6
    assertBool "Range-063" $ Range.same expected063 $ Range.num (0, 6) 3

    assertEqual "origin-060" 0 $ Range.origin expected060
    assertEqual "origin-066" 6 $ Range.origin expected066
    assertEqual "origin-063" 3 $ Range.origin expected063
  where
    expected060, expected066, expected063 :: Range Word
    expected060 = Range {lo = 0, hi = 6, offset = Offset (0 :: Word), inverted = False}
    expected066 = Range {lo = 0, hi = 6, offset = Offset (6 :: Word), inverted = False}
    expected063 = Range {lo = 0, hi = 6, offset = Offset (3 :: Word), inverted = False}
