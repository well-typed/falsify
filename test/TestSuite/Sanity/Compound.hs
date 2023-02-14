module TestSuite.Sanity.Compound (tests) where

import Data.List.NonEmpty (NonEmpty((:|)), nub)
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify.Generator (Gen)

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.Range      as Range
import qualified Test.Falsify.SampleTree as SampleTree

import TestSuite.Util.Predicates

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Compound" [
      testCase "listTowardsShorter" test_listTowardsShorter
    , testCase "listTowardsLonger"  test_listTowardsLonger
    , testCase "listTowardsOrigin"  test_listTowardsOrigin
    ]

test_listTowardsShorter :: Assertion
test_listTowardsShorter = do
    -- Note that [6, 4] is indeed the minimal counter-example to a sorted list,
    -- when the elements are drawn from the range [0, 10] with origin 5, and
    -- filtered for even numbers.
    let expectedHistory = [4,6,6,6,8,4] :| [
            [4,6,8,6,6,8,4] -- the /filtered/ list can grow in size!
          , [6,8,6,6,8,4]
          , [8,6,6,8,4]
          , [6,6,8,4]
          , [6,8,4]
          , [8,4]
           ,[6,4]
           ]
    assertEqual "shrink" expectedHistory $
      nub $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 1)
  where
    gen :: Gen [Word8]
    gen = fmap (filter even) $
            Gen.list (Range.between (10, 20)) $
              Gen.integral $ Range.num (0, 10) 5

    prop :: [Word8] -> Bool
    prop = pairwiseAll (<=)


test_listTowardsLonger :: Assertion
test_listTowardsLonger = do
    let expectedHistory = [0,1,1,1,1,0,1] :| [
            [0,1,1,1,1,0,1,0,1,1] -- we increase the list length to max immediately
          , [0,0,1,1,1,0,1,0,1,1] -- .. and then shrink towards exactly one 1
          , [0,0,0,1,1,0,1,0,1,1]
          , [0,0,0,0,1,0,1,0,1,1]
          , [0,0,0,0,0,0,1,0,1,1]
          , [0,0,0,0,0,0,1,0,0,0]
          ]
    assertEqual "shrink" expectedHistory $
      nub $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 3)
  where
    gen :: Gen [Word8]
    gen = Gen.list (Range.invert $ Range.between (0, 10)) $
            Gen.integral $ Range.between (0, 1)

    prop :: [Word8] -> Bool
    prop = pairwiseAll (<=)


test_listTowardsOrigin :: Assertion
test_listTowardsOrigin = do
    let expectedHistory = [1,0] :| [[1,0,0,1,0],[0,0,0,1,0]]
    assertEqual "shrink" expectedHistory $
      nub $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 8)
  where
    gen :: Gen [Word8]
    gen = Gen.list (Range.num (0, 10) 5) $
            Gen.integral $ Range.between (0, 1)

    prop :: [Word8] -> Bool
    prop = pairwiseAll (<=)

