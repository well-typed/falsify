module Demo.Functions (tests) where

import Data.Default
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify.Predicate ((.$))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

tests :: TestTree
tests = testGroup "Demo.Functions" [
      testPropertyWith expectFailure "listToBool" prop_listToBool
    ]
  where
    expectFailure :: TestOptions
    expectFailure = def {
          expectFailure   = ExpectFailure
        , overrideVerbose = Just Verbose
        }

prop_listToBool :: Property ()
prop_listToBool = do
    Fn (f :: [Word8] -> Bool) <- gen $ Gen.fun (Gen.bool False)
    assert $ P.eq .$ ("lhs", f [3, 1, 4, 2])
                  .$ ("rhs", f [1, 6, 1, 8])


