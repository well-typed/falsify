module TestSuite.Sanity.Range (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Falsify.Range as Range
import Test.Falsify.Range

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Range" [
      testCase "num" test_num
    ]

test_num :: Assertion
test_num = do
    assertEqual "Range-060" expected060 $ Range.num (0, 6) 0
    assertEqual "Range-066" expected066 $ Range.num (0, 6) 6
    assertEqual "Range-063" expected063 $ Range.num (0, 6) 3

    assertEqual "origin-060" 0 $ Range.origin expected060
    assertEqual "origin-066" 6 $ Range.origin expected066
    assertEqual "origin-063" 3 $ Range.origin expected063
  where
    expected060, expected066, expected063 :: Range Word Word
    expected060 = Range {lo = 0, hi = 6, offset = 0, inverted = False}
    expected066 = Range {lo = 0, hi = 6, offset = 6, inverted = False}
    expected063 = Range {lo = 0, hi = 6, offset = 3, inverted = False}
