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

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Function" [
      testGroup "BoolToBool" [
          testProperty "notConstant" prop_BoolToBool_notConstant
        , testProperty "constant"    prop_BoolToBool_constant
        ]
    , testGroup "Word8ToBool" [
          testProperty "constant" prop_Word8ToBool_constant
        ]
    , testGroup "IntegerToBool" [
          testPropertyWith (def { overrideNumTests = Just 10 })
            "constant" prop_IntegerToBool_constant
        ]
    ]

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

prop_Word8ToBool_constant :: Property ()
prop_Word8ToBool_constant = do
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

prop_IntegerToBool_constant :: Property ()
prop_IntegerToBool_constant =
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
