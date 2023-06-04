module Main (main) where

import Test.Tasty

import qualified TestSuite.Sanity.Predicate
import qualified TestSuite.Sanity.Range
import qualified TestSuite.Sanity.Selective

import qualified TestSuite.Prop.Generator.Compound
import qualified TestSuite.Prop.Generator.Function
import qualified TestSuite.Prop.Generator.Marking
import qualified TestSuite.Prop.Generator.Precision
import qualified TestSuite.Prop.Generator.Prim
import qualified TestSuite.Prop.Generator.Selective
import qualified TestSuite.Prop.Generator.Shrinking
import qualified TestSuite.Prop.Generator.Simple
import qualified TestSuite.Prop.Generator.Text

main :: IO ()
main = defaultMain $ testGroup "falsify" [
      testGroup "Sanity" [
          TestSuite.Sanity.Range.tests
        , TestSuite.Sanity.Selective.tests
        , TestSuite.Sanity.Predicate.tests
        ]
    , testGroup "Prop" [
          TestSuite.Prop.Generator.Prim.tests
        , TestSuite.Prop.Generator.Selective.tests
        , TestSuite.Prop.Generator.Marking.tests
        , TestSuite.Prop.Generator.Precision.tests
        , TestSuite.Prop.Generator.Simple.tests
        , TestSuite.Prop.Generator.Shrinking.tests
        , TestSuite.Prop.Generator.Text.tests
        , TestSuite.Prop.Generator.Compound.tests
        , TestSuite.Prop.Generator.Function.tests
        ]
    ]
