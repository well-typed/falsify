module TestSuite.Sanity.Auxiliary (tests) where

import Control.Monad
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.QuickCheck as QuickCheck

import Data.Falsify.List (pairwiseAll)
import Test.Falsify.Debugging
import Test.Falsify.Generator (Gen)

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.SampleTree as SampleTree

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Auxiliary" [
      testCase "shrinkTo"       test_shrinkTo
    , testCase "firstThen"      test_firstThen
    , testGroup "shrinkWith" [
          testCase "word" test_shrinkWith_word
        , testGroup "list" [
                testCase (show i) $ test_shrinkWith_list i
              | i <- [0,  20,  40,  60,  80, 100, 120, 140, 160, 180]
              ]
        ]
    ]

{-------------------------------------------------------------------------------
  User-specified shrinking
-------------------------------------------------------------------------------}

test_shrinkTo :: Assertion
test_shrinkTo = do
    -- Binary search is not guaranteed to always find the minimum value. For
    -- example, if we are looking for counter-examples to the property that
    -- "all numbers are even", and we start with 3, then binary search will
    -- only try 0 and 2, both of which are even, and hence conclude that
    -- 3 is the minimum counter-example. This is true in QuickCheck, also.
    assertEqual "standard shrinking" 3 $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 1)

    -- But if we try /all/ smaller numbers, this cannot happen
    let expectedHistory = [3,1]
    assertEqual "shrinkTo" expectedHistory $
      shrink (not . prop) gen' (SampleTree.fromSeed 2)
  where
    gen :: Gen Word64
    gen = Gen.prim

    gen' :: Gen Word64
    gen' = Gen.shrinkToOneOf 3 [0 .. 2]

    prop :: Word64 -> Bool
    prop = even

test_firstThen :: Assertion
test_firstThen = do
    -- The behaviour of firstThen is independent of the seed.
    forM_ [0 .. 100] $ \seed ->
      assertEqual (show seed)[True,False] $
        shrink (const True) gen (SampleTree.fromSeed seed)
  where
    gen :: Gen Bool
    gen = Gen.firstThen True False

{-------------------------------------------------------------------------------
  Test shrinkWith

  We test this for lists of increasing size, to verify that this is not growing
  exponentially with the size of the list (and thereby verifying that we are
  not exploring the full shrink tree of those lists, because they certainly
  /are/ exponential in size).

  Currently these times look something like this:

  > list length | time (ms)
  > -----------------------
  >          20    0240
  >          40    0460
  >          60    0760
  >          80    1080
  >         100    1480
  >         120    1960
  >         140    2410
  >         160    3100
  >         180    3750
  >         200    4200
  >         300    8080
  >         400   11010
  >         500   13940

  This grows with O(n^2), but that is expected (see 'shrinkWith').
-------------------------------------------------------------------------------}

test_shrinkWith_word :: Assertion
test_shrinkWith_word =
    assertEqual "" 3 $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 2)
  where
    gen :: Gen Word64
    gen = Gen.shrinkWith QuickCheck.shrink Gen.prim

    prop :: Word64 -> Bool
    prop = even

test_shrinkWith_list :: Word64 -> Assertion
test_shrinkWith_list listLength = do
    forM_ [0..1000] $ \seed -> do
      unless (prop $ run gen (SampleTree.fromSeed seed)) $
        assertEqual (show seed) [1,0] $
          last $ shrink (not . prop) gen (SampleTree.fromSeed seed)
  where
    -- NOTE: `mod` is normally /NOT/ a valid way to generate a value in a
    -- smaller range, since this will not shrink correcty when using internal
    -- shrinking. With manual shrinking, however, this does not matter.
    gen :: Gen [Word64]
    gen =
        Gen.shrinkWith QuickCheck.shrink $
          replicateM (fromIntegral listLength) $
            (`mod` 10) <$> Gen.prim

    prop :: [Word64] -> Bool
    prop = pairwiseAll (<=)