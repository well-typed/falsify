module TestSuite.Prop.Generator.Shrinking (tests) where

import Control.Monad
import Data.Default
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import qualified Test.QuickCheck as QuickCheck

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

import TestSuite.Util.List

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Shrinking" [
      testGroup "prim" [
        testPropertyWith expectFailure  "prim" prop_prim_minimum
      ]
    , testGroup "shrinkTo" [
          testProperty "shrinking" prop_shrinkTo_shrinking
        , testProperty "minimum"   prop_shrinkTo_minimum
        ]
    , testGroup "firstThen" [
          testProperty "shrinking" prop_firstThen_shrinking
        , testProperty "minimum"   prop_firstThen_minimum
        ]
    , testGroup "shrinkWith" [
          testGroup "minimum" [
              testProperty "minimum" prop_shrinkWith_minimum_word
            , testGroup "list" [
                   testProperty (show i) $ prop_shrinkWith_minimum_list i
                 | i <- [20,  40,  60,  80, 100, 120, 140, 160, 180]
                 ]
            ]
        ]
    ]
  where
    expectFailure :: TestOptions
    expectFailure = def {
          expectFailure    = ExpectFailure
        , overrideNumTests = Just 10_000
        }

{-------------------------------------------------------------------------------
  prim
-------------------------------------------------------------------------------}

-- Binary search is not guaranteed to always find the minimum value. For
-- example, if we are looking for counter-examples to the property that "all
-- numbers are even", and we start with 3, then binary search will only try 0
-- and 2, both of which are even, and hence conclude that 3 is the minimum
-- counter-example. This is true in QuickCheck, also.
prop_prim_minimum :: Property ()
prop_prim_minimum =
    testMinimum (P.expect 1) $ do
      x <- gen Gen.prim
      unless (even x) $ testFailed x

{-------------------------------------------------------------------------------
  shrinkTo
-------------------------------------------------------------------------------}

prop_shrinkTo_shrinking :: Property ()
prop_shrinkTo_shrinking =
   testShrinkingOfGen (P.relatedBy ("validShrink", validShrink)) $
     Gen.shrinkToOneOf 3 [0 :: Word .. 2]
  where
    -- 'shrinkToOneOf' only shrinks /once/, so the original (pre-shrink) value
    -- /must/ be 3.
    validShrink :: Word -> Word -> Bool
    validShrink 3 0 = True
    validShrink 3 1 = True
    validShrink 3 2 = True
    validShrink _ _ = False

prop_shrinkTo_minimum :: Property ()
prop_shrinkTo_minimum =
    testMinimum (P.expect 1) $ do
      x <- gen $ Gen.shrinkToOneOf 3 [0 :: Word .. 2]
      unless (even x) $ testFailed x

{-------------------------------------------------------------------------------
  firstThen
-------------------------------------------------------------------------------}

prop_firstThen_shrinking :: Property ()
prop_firstThen_shrinking =
   testShrinkingOfGen (P.relatedBy ("validShrink", validShrink)) $
     Gen.firstThen True False
  where
    validShrink :: Bool -> Bool -> Bool
    validShrink True False = True
    validShrink _    _     = False

prop_firstThen_minimum :: Property ()
prop_firstThen_minimum =
    testMinimum (P.expect False) $ do
      x <- gen $ Gen.firstThen True False
      testFailed x

{-------------------------------------------------------------------------------
  shrinkWith
-------------------------------------------------------------------------------}

-- This is obviously not a valid general-purpose shrinking function for
-- 'Word64', but that is not important here.
shrinkWord :: Word64 -> [Word64]
shrinkWord n = takeWhile (< n) [0 .. 100]

prop_shrinkWith_minimum_word :: Property ()
prop_shrinkWith_minimum_word =
    testMinimum (P.expect 1) $ do
      x <- gen $ Gen.shrinkWith shrinkWord Gen.prim
      unless (even x) $ testFailed x

-- | Test performance of 'shrinkWith'
--
-- We test this for lists of increasing size, to verify that this is not growing
-- exponentially with the size of the list (and thereby verifying that we are
-- not exploring the full shrink tree of those lists, because they certainly
-- /are/ exponential in size).
prop_shrinkWith_minimum_list :: Int -> Property ()
prop_shrinkWith_minimum_list listLength =
    testMinimum (P.expect [1,0]) $ do
      xs <- gen $ Gen.shrinkWith (QuickCheck.shrinkList shrinkWord) $
              replicateM listLength Gen.prim
      unless (pairwiseAll (<=) xs) $ testFailed xs



