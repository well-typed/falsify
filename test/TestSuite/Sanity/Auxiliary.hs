module TestSuite.Sanity.Auxiliary (tests) where

import Data.List.NonEmpty (NonEmpty((:|)), nub)
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Generator.Debugging
import Test.Falsify.SampleTree (SampleTree)

import qualified Test.Falsify.Generator as Gen

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Auxiliary" [
      testCase "fraction"       test_fraction
    , testCase "signedWord63"   test_signedWord63
    , testCase "signedFraction" test_signedFraction
    ]

test_fraction :: Assertion
test_fraction = do
    assertEqual "run minBound" (Fraction 0) $
      Gen.run gen (tree minBound)
    assertEqual "run maxBound" (Fraction 1) $
      Gen.run gen (tree maxBound)

    let shrinkHistory = 100 :| [
            50
          , 25
          , 12
          , 11
          , 10
          ]
    assertEqual "shrink" shrinkHistory $
      -- we nub the result: we have a bunch of fractions that all round to 10
      nub $ fmap pct $ Gen.shrink (not . prop) gen (tree maxBound)
  where
    gen :: Gen Fraction
    gen = fraction

    tree :: Word64 -> SampleTree
    tree x = expandTruncated $ B (S x) E

    prop :: Fraction -> Bool
    prop f = pct f < 10

    pct :: Fraction -> Word
    pct (Fraction f) = round (f * 100)

test_signedWord63 :: Assertion
test_signedWord63 = do
    assertEqual "run minBound" (Pos minBound) $
      Gen.run gen $ tree minBound
    assertEqual "run minBound+1" (Neg minBound) $
      Gen.run gen $ tree (minBound + 1)
    assertEqual "run minBound+2" (Pos (minBound + 1)) $
      Gen.run gen $ tree (minBound + 2)
    assertEqual "run minBound+3" (Neg (minBound + 1)) $
      Gen.run gen $ tree (minBound + 3)

    assertEqual "run maxBound" (Neg maxBound) $
      Gen.run gen $ tree maxBound
    assertEqual "run maxBound-1" (Pos maxBound) $
      Gen.run gen $ tree (maxBound - 1)
    assertEqual "run maxBound-2" (Neg (maxBound - 1)) $
      Gen.run gen $ tree (maxBound - 2)
    assertEqual "run maxBound-3" (Pos (maxBound - 1)) $
      Gen.run gen $ tree (maxBound - 3)
  where
    gen :: Gen (Signed Word63)
    gen = signedWord63

    tree :: Word64 -> SampleTree
    tree x = expandTruncated $ B (S x) E

test_signedFraction :: Assertion
test_signedFraction = do
    assertEqual "run minBound" 0 $
      Gen.run gen (tree minBound)
    assertEqual "run maxBound" (-100) $
      Gen.run gen (tree maxBound)

    let shrinkHistory = -100 :| [50,25,12,11,10]
    assertEqual "shrink" shrinkHistory $
      nub $ Gen.shrink (not . prop) gen (tree maxBound)
  where
    gen :: Gen Int
    gen = toPercentage <$> signedFraction

    tree :: Word64 -> SampleTree
    tree x = expandTruncated $ B (B (B (S x) E) E) E

    prop :: Int -> Bool
    prop pct = abs pct < 10

    toPercentage :: Signed Fraction -> Int
    toPercentage (Pos (Fraction f)) = round (f * 100)
    toPercentage (Neg (Fraction f)) = negate $ round (f * 100)
