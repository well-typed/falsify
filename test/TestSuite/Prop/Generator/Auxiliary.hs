module TestSuite.Prop.Generator.Auxiliary (tests) where

import Control.Monad
import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify.Generator (WordN(..))
import Test.Falsify.Range (Precision(..), ProperFraction(..))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Auxiliary" [
      testGroup "wordN" [
          testGroup (show p) [
              testProperty "shrinking" $ prop_wordN_shrinking p
            , testProperty "minimum"   $ prop_wordN_minimum   p
            ]
        | p <- map Precision [0, 1, 2, 3, 63, 64]
        ]
    , testGroup "fraction" [
          testGroup (show p) [
              testProperty "shrinking" $ prop_fraction_shrinking (Precision p)
            , testProperty "minimum"   $ prop_fraction_minimum   (Precision p) target expected
            ]
        | (p, target, expected) <- [
             -- The higher the precision, the closer we can get to the target
             (2  , 50, 75)
           , (3  , 50, 62)
           , (4  , 50, 56)
           , (5  , 50, 53)
           , (63 , 50, 51)
           , (64 , 50, 51)
           ]
        ]
    ]

{-------------------------------------------------------------------------------
  wordN
-------------------------------------------------------------------------------}

prop_wordN_shrinking :: Precision -> Property ()
prop_wordN_shrinking p =
    testShrinkingOfGen P.ge $ Gen.wordN p

prop_wordN_minimum :: Precision -> Property ()
prop_wordN_minimum p =
    testMinimum (P.expect $ WordN p 0) $ do
      x <- gen $ Gen.wordN p
      testFailed x

{-------------------------------------------------------------------------------
  fraction
-------------------------------------------------------------------------------}

prop_fraction_shrinking :: Precision -> Property ()
prop_fraction_shrinking p =
    testShrinkingOfGen P.ge $ Gen.properFraction p

prop_fraction_minimum :: Precision -> Word -> Word -> Property ()
prop_fraction_minimum p target expected =
    testMinimum ((P.expect expected) `P.dot` P.fn ("pct", pct)) $ do
      x <- gen $ Gen.properFraction p
      unless (pct x <= target) $ testFailed x
  where
    pct :: ProperFraction -> Word
    pct (ProperFraction f) = round (f * 100)

