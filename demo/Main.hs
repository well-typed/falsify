module Main (main) where

import Test.Tasty

import qualified Demo.Simple
import qualified Demo.HowToSpecifyIt

main :: IO ()
main = defaultMain $ testGroup "demo" [
      Demo.Simple.tests
    , Demo.HowToSpecifyIt.tests
    ]
