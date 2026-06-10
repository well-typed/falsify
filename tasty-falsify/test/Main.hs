module Main (main) where

import Test.Tasty

import qualified TestSuite.Generator.Compound
import qualified TestSuite.Generator.Function
import qualified TestSuite.Generator.Marking
import qualified TestSuite.Generator.Precision
import qualified TestSuite.Generator.Prim
import qualified TestSuite.Generator.Selective
import qualified TestSuite.Generator.Shrinking
import qualified TestSuite.Generator.Simple

main :: IO ()
main = defaultMain $ testGroup "falsify" [
      testGroup "Generator" [
          TestSuite.Generator.Prim.tests
        , TestSuite.Generator.Selective.tests
        , TestSuite.Generator.Marking.tests
        , TestSuite.Generator.Precision.tests
        , TestSuite.Generator.Simple.tests
        , TestSuite.Generator.Shrinking.tests
        , TestSuite.Generator.Compound.tests
        , TestSuite.Generator.Function.tests
        ]
    ]
