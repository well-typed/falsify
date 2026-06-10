{-# LANGUAGE OverloadedStrings #-}

-- | Predicates
--
-- This demo matches the module documentation for "Test.Falsify.Predicate".
module Demo.Predicates (tests) where

import Control.Monad
import Test.Tasty
import Test.Tasty.Falsify

import qualified Test.Tasty.ExpectedFailure as Tasty
import qualified Test.Tasty.HUnit           as HUnit
import qualified Test.Tasty.QuickCheck      as QuickCheck

import Test.Falsify
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
              testProperty "bool"  test_falsify_bool
            , testProperty "equal" test_falsify_equal
            ]
        ]
    , testGroup "Introduction" [
          testProperty "even1"         test_even1
        , testProperty "even2"         test_even2
        , testProperty "equal"         test_equal
        , testProperty "samePolarity1" test_samePolarity1
        , testProperty "samePolarity2" test_samePolarity2
        , testProperty "samePolarity3" test_samePolarity3
        , testProperty "samePolarity4" test_samePolarity4
        ]
    , testGroup "Other" [
          testProperty "realVsModel" test_realVsModel
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

test_falsify_bool :: Property ()
test_falsify_bool = when (x /= y) $ testFailed "uhoh"

test_falsify_equal :: Property ()
test_falsify_equal = assert $ P.eq .$ ("x", x) .$ ("y", y)

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

{-------------------------------------------------------------------------------
  N-ary predicates
-------------------------------------------------------------------------------}

type Policy    = String
type Operation = String
type Resource  = String
type Actor     = String

policy :: Policy
policy = "strict"

operation :: Operation
operation = "delete"

resource :: Resource
resource = "db"

actor :: Actor
actor = "joe"

applyReal, applyModel :: Policy -> Operation -> Resource -> Actor -> Bool
applyReal  _ _ _ _ = False
applyModel _ _ _ _ = True

realVsModel :: Predicate '[Policy, Operation, Resource, Actor]
realVsModel = P.lam $ \p -> P.lam $ \o -> P.lam $ \r -> P.lam $ \a ->
    let real  = applyReal  p o r a
        model = applyModel p o r a
    in if real == model then
         P.pass
       else
         P.fail $ "real says " ++ show real ++ ", model says " ++ show model

test_realVsModel :: Property ()
test_realVsModel = assert $
    realVsModel
      .$ ( "policy"    , policy    )
      .$ ( "operation" , operation )
      .$ ( "resource"  , resource  )
      .$ ( "actor"     , actor     )
