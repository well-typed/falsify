module Demo.Functions (tests) where

import Data.Default
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import qualified Test.Falsify.Generator as Gen

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
    assertBool $ f [3, 1, 4, 2] == f [1, 6, 1, 8]


