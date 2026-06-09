{-# LANGUAGE OverloadedStrings #-}

-- | Predicates
--
-- This demo matches the module documentation for "Test.Falsify.Predicate".
module Demo.Predicates (tests) where

import Control.Monad
import Test.Tasty

import qualified Test.Tasty.ExpectedFailure as Tasty
import qualified Test.Tasty.Falsify         as Falsify
import qualified Test.Tasty.HUnit           as HUnit
import qualified Test.Tasty.QuickCheck      as QuickCheck

import Test.Tasty.Falsify
import Test.Falsify.Predicate (Predicate, (.$))

import qualified Test.Falsify.Predicate as P

{-------------------------------------------------------------------------------
  Overview
-------------------------------------------------------------------------------}

tests :: TestTree
tests = Tasty.expectFail $ testGroup "Demo.Predicates" [
      testGroup "Motivation" [
          testGroup "HUnit" [
              HUnit.testCase "bool"  test_hunit_bool
            , HUnit.testCase "equal" test_hunit_equal
            ]
        , testGroup "QuickCheck" [
              QuickCheck.testProperty "bool"  test_qc_bool
            , QuickCheck.testProperty "equal" test_qc_equal
            ]
        , testGroup "falsify" [
              Falsify.testProperty "bool"  test_falsify_bool
            , Falsify.testProperty "equal" test_falsify_equal
            ]
        ]
    , testGroup "Introduction" [
          Falsify.testProperty "even1"         test_even1
        , Falsify.testProperty "even2"         test_even2
        , Falsify.testProperty "equal"         test_equal
        , Falsify.testProperty "samePolarity1" test_samePolarity1
        , Falsify.testProperty "samePolarity2" test_samePolarity2
        , Falsify.testProperty "samePolarity3" test_samePolarity3
        , Falsify.testProperty "samePolarity4" test_samePolarity4
        ]
    ]

{-------------------------------------------------------------------------------
  Motivation: HUnit and QuickCheck vs falsify
-------------------------------------------------------------------------------}

x, y :: Int
x = 5
y = 10

test_hunit_bool :: HUnit.Assertion
test_hunit_bool = HUnit.assertBool "uhoh" $ x == y

test_hunit_equal :: HUnit.Assertion
test_hunit_equal = HUnit.assertEqual "uhoh" x y

test_qc_bool :: QuickCheck.Property
test_qc_bool = QuickCheck.property $ x == y

test_qc_equal :: QuickCheck.Property
test_qc_equal = QuickCheck.property $ x QuickCheck.=== y

test_falsify_bool :: Falsify.Property ()
test_falsify_bool = when (x /= y) $ Falsify.testFailed "uhoh"

test_falsify_equal :: Falsify.Property ()
test_falsify_equal = Falsify.assert $ P.eq .$ ("x", x) .$ ("y", y)

{-------------------------------------------------------------------------------
  Introduction to predicates
-------------------------------------------------------------------------------}

even1 :: Integral a => Predicate '[a]
even1 = P.unary even $ \a -> "not even: " ++ P.prettyExpr a

test_even1 :: Property ()
test_even1 = assert $ even1 `P.at` ("x", show x, x)

test_even2 :: Property ()
test_even2 = assert $ even1 .$ ("x", x)

test_equal :: Property ()
test_equal = assert $
    P.eq .$ ("x", x)
         .$ ("y", y)

test_samePolarity1 :: Property ()
test_samePolarity1 = assert $
    P.eq .$ ("even x", even x)
         .$ ("even y", even y)

samePolarity :: Integral a => Predicate '[a, a]
samePolarity =
    P.binary
      (\a b -> even a == even b)
      (\a b -> P.prettyExpr a ++ " and " ++ P.prettyExpr b ++ " have different polarity")

test_samePolarity2 :: Property ()
test_samePolarity2 = assert $ samePolarity .$ ("x", x) .$ ("y", y)

samePolarity' :: Integral a => Predicate '[a, a]
samePolarity' = P.eq `P.on` P.fn ("even", even)

test_samePolarity3 :: Property ()
test_samePolarity3 = assert $
    samePolarity'
      .$ ("x", x)
      .$ ("y", y)

newtype T = WrapT Int
  deriving stock (Show)

unwrapT :: T -> Int
unwrapT (WrapT a) = a

test_samePolarity4 :: Property ()
test_samePolarity4 = assert $
    samePolarity' `P.on` P.transparent unwrapT
      .$ ("x", WrapT 5)
      .$ ("y", WrapT 10)
