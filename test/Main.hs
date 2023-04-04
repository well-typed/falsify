module Main (main) where

import Test.Tasty

import qualified TestSuite.Sanity.Auxiliary
import qualified TestSuite.Sanity.Compound
import qualified TestSuite.Sanity.Function
import qualified TestSuite.Sanity.Predicate
import qualified TestSuite.Sanity.Prim
import qualified TestSuite.Sanity.Selective

import qualified TestSuite.Prop.Generator.Auxiliary
import qualified TestSuite.Prop.Generator.Compound
import qualified TestSuite.Prop.Generator.Function
import qualified TestSuite.Prop.Generator.Marking
import qualified TestSuite.Prop.Generator.Prim
import qualified TestSuite.Prop.Generator.Simple

main :: IO ()
main = defaultMain $ testGroup "falsify" [
      testGroup "Sanity" [
          TestSuite.Sanity.Prim.tests
        , TestSuite.Sanity.Auxiliary.tests
        , TestSuite.Sanity.Compound.tests
        , TestSuite.Sanity.Selective.tests
        , TestSuite.Sanity.Function.tests
        , TestSuite.Sanity.Predicate.tests
        ]
    , testGroup "Prop" [
          TestSuite.Prop.Generator.Prim.tests
        , TestSuite.Prop.Generator.Marking.tests
        , TestSuite.Prop.Generator.Auxiliary.tests
        , TestSuite.Prop.Generator.Simple.tests
        , TestSuite.Prop.Generator.Compound.tests
        , TestSuite.Prop.Generator.Function.tests
        ]
    ]
