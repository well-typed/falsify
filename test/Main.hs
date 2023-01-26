module Main where

import Test.Tasty

import qualified TestSuite.Sanity.Generator

main :: IO ()
main = defaultMain $ testGroup "falsify" [
      TestSuite.Sanity.Generator.tests
    ]
