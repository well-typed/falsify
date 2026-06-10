module Main (main) where

import Test.Tasty
import Test.Tasty.Falsify
import Test.Falsify

main :: IO ()
main = defaultMain $ testGroup "MyTestSuite" [
      testProperty "myFirstProperty" prop_myFirstProperty
    ]

prop_myFirstProperty :: Property ()
prop_myFirstProperty = return ()
