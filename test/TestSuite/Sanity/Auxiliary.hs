module TestSuite.Sanity.Auxiliary (tests) where

import Control.Monad
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List.NonEmpty as NE
import qualified Test.QuickCheck    as QuickCheck

import Data.Falsify.List (pairwiseAll)
import Test.Falsify.Debugging
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator.Auxiliary
import Test.Falsify.SampleTree (SampleTree, Sample (..))

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.SampleTree as SampleTree

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Auxiliary" [
      testCase "fraction"       test_fraction
    , testCase "signedWordN"    test_signedWordN
    , testCase "signedFraction" test_signedFraction
    , testCase "shrinkTo"       test_shrinkTo
    , testCase "firstThen"      test_firstThen
    , testGroup "shrinkWith" [
          testCase "word" test_shrinkWith_word
        , testGroup "list" [
                testCase (show i) $ test_shrinkWith_list i
              | i <- [   0,  20,  40,  60,  80
                     , 100, 120, 140, 160, 180
--                     , 200
--                     , 300
--                     , 400
--                     , 500
                     ]
              ]
        ]
    ]

{-------------------------------------------------------------------------------
  Fractions
-------------------------------------------------------------------------------}

test_fraction :: Assertion
test_fraction = do
    assertEqual "run minBound" (Fraction 0) $
      Gen.run gen (tree minBound)
    assertEqual "run maxBound" (Fraction 1) $
      Gen.run gen (tree maxBound)

    let expectedHistory = 100 :| [52,26,13,10]
    assertEqual "shrink" expectedHistory $
      fmap pct $ Gen.shrink (not . prop) gen (tree maxBound)
  where
    gen :: Gen Fraction
    gen = fraction 5

    tree :: Word64 -> SampleTree
    tree x = expandTruncated $ S (NotShrunk x)

    prop :: Fraction -> Bool
    prop f = pct f < 10

    pct :: Fraction -> Word
    pct (Fraction f) = round (f * 100)

test_signedWordN :: Assertion
test_signedWordN = do
    assertEqual "run minBound" (Pos $ WordN 4 0) $
      Gen.run gen $ tree 0
    assertEqual "run minBound+1" (Neg $ WordN 4 0) $
      Gen.run gen $ tree 1
    assertEqual "run minBound+2" (Pos $ WordN 4 1) $
      Gen.run gen $ tree 2
    assertEqual "run minBound+3" (Neg $ WordN 4 1) $
      Gen.run gen $ tree 3

    assertEqual "run maxBound" (Neg $ WordN 4 15) $
      Gen.run gen $ tree 31 -- need 5 bits of precision for signed 4-bit number
    assertEqual "run maxBound+1" (Neg $ WordN 4 15) $
      Gen.run gen $ tree 31
    assertEqual "run maxBound-1" (Pos $ WordN 4 15) $
      Gen.run gen $ tree 30
    assertEqual "run maxBound-2" (Neg $ WordN 4 14) $
      Gen.run gen $ tree 29
    assertEqual "run maxBound-3" (Pos $ WordN 4 14) $
      Gen.run gen $ tree 28
  where
    gen :: Gen (Signed WordN)
    gen = signedWordN 4

    tree :: Word64 -> SampleTree
    tree x = expandTruncated $ S (NotShrunk x)

test_signedFraction :: Assertion
test_signedFraction = do
    assertEqual "run minBound" 0 $
      Gen.run gen (tree minBound)
    assertEqual "run maxBound" (-100) $
      Gen.run gen (tree maxBound)

    let expectedHistory = -100 :| [52,-26,13,-10,10]
    assertEqual "shrink" expectedHistory $
      Gen.shrink (not . prop) gen (tree maxBound)
  where
    gen :: Gen Int
    gen = toPercentage <$> signedFraction 5

    tree :: Word64 -> SampleTree
    tree x = expandTruncated $ S (NotShrunk x)

    prop :: Int -> Bool
    prop pct = abs pct < 10

    toPercentage :: Signed Fraction -> Int
    toPercentage (Pos (Fraction f)) = round (f * 100)
    toPercentage (Neg (Fraction f)) = negate $ round (f * 100)

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
      NE.last $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 1)

    -- But if we try /all/ smaller numbers, this cannot happen
    let expectedHistory = 3 :| [1]
    assertEqual "shrinkTo" expectedHistory $
      Gen.shrink (not . prop) gen' (SampleTree.fromSeed 2)
  where
    gen :: Gen Word64
    gen = Gen.prim

    gen' :: Gen Word64
    gen' = shrinkTo 3 [0 .. 2]

    prop :: Word64 -> Bool
    prop = even

test_firstThen :: Assertion
test_firstThen = do
    -- The behaviour of firstThen is independent of the seed.
    forM_ [0 .. 100] $ \seed ->
      assertEqual (show seed) (True :| [False]) $
        Gen.shrink (const True) gen (SampleTree.fromSeed seed)
  where
    gen :: Gen Bool
    gen = firstThen True False

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
      NE.last $ Gen.shrink (not . prop) gen (SampleTree.fromSeed 2)
  where
    gen :: Gen Word64
    gen = shrinkWith QuickCheck.shrink Gen.prim

    prop :: Word64 -> Bool
    prop = even

test_shrinkWith_list :: Word64 -> Assertion
test_shrinkWith_list listLength = do
    forM_ [0..1000] $ \seed -> do
      unless (prop $ Gen.run gen (SampleTree.fromSeed seed)) $
        assertEqual (show seed) [1,0] $
          NE.last $ Gen.shrink (not . prop) gen (SampleTree.fromSeed seed)
  where
    gen :: Gen [Word64]
    gen = shrinkWith QuickCheck.shrink $ do
        -- This generator generates a list of [0..99] elements, with the
        -- elements themselves ranging from [0..9]. Note that @mod@ here is
        -- NOT normally the correct way to cut-off a range: for some @x@ chosen
        -- from a larger range (here, the full Word64), @x mod n@ will NOT
        -- shrink as @x@ shrinks (but rather, it will loop over its range).
        -- However, since we override the shrinker, this doesn't matter.
        if listLength == 0 then
          return []
        else do
          n <- (`mod` listLength) <$> Gen.prim
          replicateM (fromIntegral n) $ (`mod` 10) <$> Gen.prim

    prop :: [Word64] -> Bool
    prop = pairwiseAll (<=)