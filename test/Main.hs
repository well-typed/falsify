module Main (main) where

import Test.Tasty

import qualified TestSuite.Sanity.Auxiliary
import qualified TestSuite.Sanity.Compound
import qualified TestSuite.Sanity.Function
import qualified TestSuite.Sanity.Prim
import qualified TestSuite.Sanity.Range
import qualified TestSuite.Sanity.Selective
import qualified TestSuite.Sanity.Simple

import qualified TestSuite.Prop.Generator.Simple

main :: IO ()
main = defaultMain $ testGroup "falsify" [
      testGroup "Sanity" [
          TestSuite.Sanity.Prim.tests
        , TestSuite.Sanity.Range.tests
        , TestSuite.Sanity.Auxiliary.tests
        , TestSuite.Sanity.Simple.tests
        , TestSuite.Sanity.Compound.tests
        , TestSuite.Sanity.Selective.tests
        , TestSuite.Sanity.Function.tests
        ]
    , testGroup "Prop" [
          TestSuite.Prop.Generator.Simple.tests
        ]
    ]
