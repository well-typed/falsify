module TestSuite.Regression (tests) where

import Control.Monad
import Data.Int
import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import Test.Falsify.Interactive

{-------------------------------------------------------------------------------
  Lists of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "TestSuite.Regression" [
      testCase "issue89" test_issue89
    ]

{-------------------------------------------------------------------------------
  Specific tests
-------------------------------------------------------------------------------}

test_issue89 :: Assertion
test_issue89 = do
    replicateM_ 10 $ do
      f <- sample (Gen.fun (Gen.inRange (Range.between (0 :: Int, 100))))
      let x = 0 :: Int8
          y = Gen.applyFun f x
      assertBool "inRange" $ 0 <= y && y <= 100

