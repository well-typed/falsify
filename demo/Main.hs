module Main (main) where

import Test.Tasty

import qualified Demo.HowToSpecifyIt
import qualified Demo.Simple
import qualified Demo.TestShrinking

main :: IO ()
main = defaultMain $ testGroup "demo" [
      Demo.Simple.tests
    , Demo.TestShrinking.tests
    , Demo.HowToSpecifyIt.tests
    ]
