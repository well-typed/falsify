module TestSuite.Prop.Generator.Function (tests) where

import Control.Monad
import Data.Default
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import qualified Data.Set as Set

import Test.Falsify.Generator (Fun)

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as Range

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Function" [
      testGroup "BoolToBool" [
          testProperty "notConstant" prop_BoolToBool_notConstant
        , testProperty "constant"    prop_BoolToBool_constant
        ]
    , testProperty                "Word8ToBool"   prop_Word8ToBool
    , testPropertyWith fewerTests "IntegerToBool" prop_IntegerToBool
    , testProperty                "IntToInt"      prop_IntToInt
    , testPropertyWith fewerTests "StringToBool"  prop_StringToBool
    ]
  where
    -- These tests are pretty slow
    fewerTests :: TestOptions
    fewerTests = def {
        overrideNumTests = Just 10
      }

{-------------------------------------------------------------------------------
  Functions @Bool -> Bool@

  TODO: Should we define these in terms of the concrete functions instead?
-------------------------------------------------------------------------------}

prop_BoolToBool_notConstant :: Property ()
prop_BoolToBool_notConstant =
    testMinimum (P.satisfies ("isConstant", isConstant)) $ do
      fn <- gen $ Gen.fun (Gen.bool False)
      let Fn f = fn
      -- "No function Bool -> Bool can be constant"
      unless (f False /= f True) $ testFailed fn
  where
    isConstant :: Fun Bool Bool -> Bool
    isConstant fn = show fn == "{_->False}"

prop_BoolToBool_constant :: Property ()
prop_BoolToBool_constant = do
    testMinimum (P.satisfies ("notConstant", notConstant)) $ do
      fn <- gen $ Gen.fun (Gen.bool False)
      let Fn f = fn
      -- "Every function Bool -> Bool is constant"
      unless (f False == f True) $ testFailed fn
  where
    notConstant :: Fun Bool Bool -> Bool
    notConstant fn = or [
          show fn == "{True->True, _->False}"
        , show fn == "{False->True, _->False}"
        ]

{-------------------------------------------------------------------------------
  Functions @Word8 -> Bool@
-------------------------------------------------------------------------------}

prop_Word8ToBool :: Property ()
prop_Word8ToBool = do
    testMinimum (P.satisfies ("notConstant", notConstant)) $ do
      fn <- gen $ Gen.fun (Gen.bool False)
      -- "Every function Word8 -> Bool is constant"
      unless (isConstant fn) $ testFailed fn
  where
    notConstant :: Fun Word8 Bool -> Bool
    notConstant fn = any aux [0 .. 255]
      where
        aux :: Word8 -> Bool
        aux n = show fn == "{" ++ show n ++ "->True, _->False}"

    isConstant :: Fun Word8 Bool -> Bool
    isConstant (Fn f) =
        (\s -> Set.size s == 1) $
          Set.fromList (map f [minBound .. maxBound])

{-------------------------------------------------------------------------------
  Functions @Integer -> Bool@

  This is the first test where the input domain is infinite.
-------------------------------------------------------------------------------}

prop_IntegerToBool :: Property ()
prop_IntegerToBool =
    testMinimum (P.satisfies ("expected", expected)) $ do
      fn <- gen $ Gen.fun (Gen.bool False)
      let Fn f = fn
      -- "Every fn from Integer to Bool must give the same result for π and φ"
      unless (f 3142 == f 1618) $ testFailed fn
  where
    expected :: Fun Integer Bool -> Bool
    expected fn = or [
          show fn == "{1618->True, _->False}"
        , show fn == "{3142->True, _->False}"
        ]

{-------------------------------------------------------------------------------
  Functions @Int -> Int@
-------------------------------------------------------------------------------}

prop_IntToInt :: Property ()
prop_IntToInt =
    testMinimum (P.satisfies ("expected", expected)) $ do
      fn <- gen $ Gen.fun (Gen.integral $ Range.between (0, 100))
      let Fn f = fn
      unless (f 0 == 0 && f 1 == 0) $ testFailed fn
  where
    expected :: Fun Int Int -> Bool
    expected fn = or [
          show fn == "{1->1, _->0}"
        , show fn == "{0->1, _->0}"
        ]

{-------------------------------------------------------------------------------
  Functions @String -> Bool@

  This example (as well as 'test_IntToInt_mapFilter') is adapted from
  Koen Claessen's presentation "Shrinking and showing functions"
  at Haskell Symposium 2012 <https://www.youtube.com/watch?v=CH8UQJiv9Q4>.

  TODO: His example uses longer strings, which does work, it's just expensive.
  We can definitely use some performance optimization here.
-------------------------------------------------------------------------------}

prop_StringToBool :: Property ()
prop_StringToBool =
    testMinimum (P.satisfies ("expected", expected)) $ do
      fn <- gen $ Gen.fun (Gen.bool False)
      let Fn p = fn
      unless (p "abc" `implies` p "def") $ testFailed fn
  where
    -- TODO: Actually, the second case doesn't seem to get triggered. Not sure
    -- why; maybe it's just unlikely..?
    expected :: Fun String Bool -> Bool
    expected fn = or [
          show fn == "{\"abc\"->True, _->False}"
        , show fn == "{\"def\"->True, _->False}"
        ]

    implies :: Bool -> Bool -> Bool
    implies False _ = True
    implies True  b = b
