module Demo.TestOptions (tests) where

import Data.Default
import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify.Predicate ((.$))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range
import qualified Test.Falsify.Predicate as P

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
    , testGroup "Discard" [
          testPropertyWith def
            "def"                  prop_even_discard
        , testPropertyWith (def { overrideVerbose = Just Verbose  })
            "verbose"              prop_even_discard
        , testPropertyWith (def { expectFailure   = ExpectFailure })
            "expectFailure"        prop_even_discard
        , testPropertyWith (def { overrideVerbose = Just Verbose
                                , expectFailure   = ExpectFailure })
            "verboseExpectFailure" prop_even_discard
       ]
    ]

-- | Valid property (property that holds)
--
-- "Every value between 0 and 100 is between 0 and 100"
prop_inRange :: Property ()
prop_inRange = do
    x :: Word <- gen $ Gen.integral $ Range.num (0, 100) 0
    assert $ P.between 0 100 .$ ("x", x)

-- | Invalid property (property that does not hold)
--
-- "Every value between 0 and 100 is even"
prop_even :: Property ()
prop_even = do
    x :: Word <- gen $ Gen.integral $ Range.num (0, 100) 0
    assert $ P.even .$ ("x", x)

-- | Like 'prop_even', but discarding tests that fail.
prop_even_discard :: Property ()
prop_even_discard = do
    x :: Word <- gen $ Gen.integral $ Range.num (0, 100) 0
    discardIf $ odd x
    assert $ P.even .$ ("x", x)

