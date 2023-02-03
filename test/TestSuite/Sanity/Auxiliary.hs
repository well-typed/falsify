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
    , testCase "signedWordN"    test_signedWordN
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

    -- TODO: (Here and elsewhere): Testing against these very precise trees
    -- is useful for illuminating how exactly the generator works, but it also
    -- makes these tests quite fragile. An additional `fmap` somewhere will
    -- result in a different tree. Perhaps we should generalize 'Gen' so that
    -- we can implement 'fmap' without changing the generator structure.
    tree :: Word64 -> SampleTree
    tree x = expandTruncated $ B (B (S x) E) E

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
