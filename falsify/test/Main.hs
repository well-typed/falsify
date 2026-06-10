module Main (main) where

import Test.Tasty

import qualified TestSuite.GenDefault
import qualified TestSuite.Sanity.Predicate
import qualified TestSuite.Sanity.Range
import qualified TestSuite.Sanity.Selective
import qualified TestSuite.Regression

main :: IO ()
main = defaultMain $ testGroup "falsify" [
      testGroup "Sanity" [
          TestSuite.Sanity.Range.tests
        , TestSuite.Sanity.Selective.tests
        , TestSuite.Sanity.Predicate.tests
        ]
    , TestSuite.Regression.tests
    , TestSuite.GenDefault.tests
    ]
