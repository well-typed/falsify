module TestSuite.Prop.Generator.Auxiliary (tests) where

import Control.Monad
import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Predicate ((.$))

import qualified Test.Falsify.Predicate as P

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Auxiliary" [
      testGroup "unsignedWordN" [
          testGroup (show p) [
              testProperty "shrinking" $ prop_unsignedWordN_shrinking p
            , testProperty "minimum"   $ prop_unsignedWordN_minimum   p
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
             (1  , 50, 100)
           , (2  , 50, 67)
           , (3  , 50, 57)
           , (4  , 50, 53)
           , (5  , 50, 52)
           , (63 , 50, 50)
           , (64 , 50, 50)
           ]
        ]
    ]

{-------------------------------------------------------------------------------
  unsignedWordN
-------------------------------------------------------------------------------}

prop_unsignedWordN_shrinking :: Precision -> Property ()
prop_unsignedWordN_shrinking p =
    testShrinkingOfGen P.ge $ unsignedWordN p

prop_unsignedWordN_minimum :: Precision -> Property ()
prop_unsignedWordN_minimum p =
    testMinimum (P.eq .$ ("expected", WordN p 0)) $ do
      x <- gen $ unsignedWordN p
      testFailed x

{-------------------------------------------------------------------------------
  fraction
-------------------------------------------------------------------------------}

prop_fraction_shrinking :: Precision -> Property ()
prop_fraction_shrinking p =
    testShrinkingOfGen P.ge $ fraction p

prop_fraction_minimum :: Precision -> Word -> Word -> Property ()
prop_fraction_minimum p target expected =
    testMinimum ((P.eq .$ ("expected", expected)) `P.dot` P.fn ("pct", pct)) $ do
      x <- gen $ fraction p
      unless (pct x < target) $ testFailed x
  where
    pct :: Fraction -> Word
    pct (Fraction f) = round (f * 100)

