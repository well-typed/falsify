module Demo.TestOptions (tests) where

import Data.Default
import Test.Tasty
import Test.Tasty.Falsify

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range

tests :: TestTree
tests = testGroup "Demo.Simple" [
      testGroup "Valid" [
          testPropertyWith def
            "def"                  prop_inRange
        , testPropertyWith (def { overrideVerbose = Just Verbose  })
            "verbose"              prop_inRange
        , testPropertyWith (def { expectFailure   = ExpectFailure })
            "expectFailure"        prop_inRange
        , testPropertyWith (def { overrideVerbose = Just Verbose
                                , expectFailure   = ExpectFailure })
            "verboseExpectFailure" prop_inRange
        ]
    , testGroup "Invalid" [
          testPropertyWith def
            "def"                  prop_even
        , testPropertyWith (def { overrideVerbose = Just Verbose  })
            "verbose"              prop_even
        , testPropertyWith (def { expectFailure   = ExpectFailure })
            "expectFailure"        prop_even
        , testPropertyWith (def { overrideVerbose = Just Verbose
                                , expectFailure   = ExpectFailure })
            "verboseExpectFailure" prop_even
       ]
    ]

-- | Valid property (property that holds)
--
-- "Every value between 0 and 100 is between 0 and 100"
prop_inRange :: Property ()
prop_inRange = do
    x :: Word <- gen $ Gen.integral $ Range.num (0, 100) 0
    assert ("not in range: " ++ show x) $ 0 <= x && x <= 100

-- | Invalid property (property that does not hold)
--
-- "Every value between 0 and 100 is even"
prop_even :: Property ()
prop_even = do
    x :: Word <- gen $ Gen.integral $ Range.num (0, 100) 0
    assert ("not even: " ++ show x) $ even x

