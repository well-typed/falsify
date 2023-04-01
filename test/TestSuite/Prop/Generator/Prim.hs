module TestSuite.Prop.Generator.Prim (tests) where

import Control.Monad (unless)
import Data.Default
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify.Predicate ((.$))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Prim" [
    testGroup "prim" [
        testProperty "shrinking" prop_prim_shrinking
      , testGroup "minimum" [
            testProperty (show target) $ prop_prim_minimum target
          | target <- [0 .. 4]
          ]
      , testPropertyWith (def { expectFailure = ExpectFailure })
          "prim_minimum_wrong" prop_prim_minimum_wrong
      ]
    ]


{-------------------------------------------------------------------------------
  Prim
-------------------------------------------------------------------------------}

-- Gen.prime is the only generator where we a /strict/ inequality
prop_prim_shrinking :: Property ()
prop_prim_shrinking = testShrinkingOfGen P.gt $ Gen.prim

-- The minimum is always 0, unless 0 is not a counter-example
prop_prim_minimum :: Word64 -> Property ()
prop_prim_minimum target = do
    testMinimum (P.eq .$ ("expected", if target == 0 then 1 else 0)) $ do
      x <- gen $ Gen.prim
      unless (x == target) $ testFailed x

-- | Just to verify that we if we specify the /wrong/ minimum, we get a failure
prop_prim_minimum_wrong :: Property ()
prop_prim_minimum_wrong =
    testMinimum (P.eq .$ ("expected", 1)) $ do
      x <- gen $ Gen.prim
      testFailed x
