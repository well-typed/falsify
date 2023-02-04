module Main (main) where

import Data.Default
import Test.Tasty
import Test.Tasty.Falsify

-- TODO: Some of these imports should not be necessary; Test.Tasty.Falsify
-- should (re-)export them.

import Test.Falsify.Property

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range

main :: IO ()
main = defaultMain $ testGroup "demo" [
      testProperty
        "even"                 prop_even
    , testPropertyWith (def { expectFailure = ExpectFailure })
       "even_expectFailure"    prop_even
    , testPropertyWith (def { overrideVerbose = Just Verbose })
       "inRange"               prop_inRange
    , testPropertyWith (def { expectFailure = ExpectFailure })
       "inRange_expectFailure" prop_inRange
    ]

-- | "Every value between 0 and 100 is even"
prop_even :: Property String String
prop_even = do
    x :: Word <- gen $ Gen.integral $ Range.num (0, 100) 0
    assert ("not even: " ++ show x) $ even x
    return "OK"

-- | "Every value between 0 and 100 is between 0 and 100"
prop_inRange :: Property String String
prop_inRange = do
    x :: Word <- gen $ Gen.integral $ Range.num (0, 100) 0
    assert ("not in range: " ++ show x) $ 0 <= x && x <= 100
    return "OK"

