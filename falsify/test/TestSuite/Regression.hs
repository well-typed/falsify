module TestSuite.Regression (tests) where

import Control.Monad
import Data.Int
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify
import Test.Falsify.Interactive (sample)

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range

{-------------------------------------------------------------------------------
  Lists of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "TestSuite.Regression" [
      testCase "issue81" test_issue81
    , testCase "issue89" test_issue89
    ]

{-------------------------------------------------------------------------------
  Specific tests
-------------------------------------------------------------------------------}

test_issue81 :: Assertion
test_issue81 = do
    checkNumOdd $ (length . filter odd) <$> replicateM n (genDefault @Std @Int    undefined)
    checkNumOdd $ (length . filter odd) <$> replicateM n (genDefault @Std @Int64  undefined)
    checkNumOdd $ (length . filter odd) <$> replicateM n (genDefault @Std @Word64 undefined)
    checkNumOdd $ (length . filter odd) <$> replicateM n (genDefault @Std @Word32 undefined)
    checkNumOdd $ (length . filter odd) <$> replicateM n (genDefault @Std @Int32  undefined)
  where
    n = 100000

    checkNumOdd :: Gen Int -> Assertion
    checkNumOdd g = do
        numOdd <- sample g
        -- If we generate 100,000 numbers, the probability of generating less
        -- than 1000 odd numbers is astronomically small. So if this happens,
        -- it (almost) certainly is a bug.
        assertBool "not enough odd numbers" $ numOdd > 1000

test_issue89 :: Assertion
test_issue89 = do
    replicateM_ 10 $ do
      f <- sample (Gen.fun (Gen.inRange (Range.inclusive (0 :: Int, 100))))
      let x = 0 :: Int8
          y = applyFun f x
      assertBool "inRange" $ 0 <= y && y <= 100
