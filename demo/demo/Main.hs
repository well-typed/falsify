module Main (main) where

import Test.Tasty

import qualified Demo.Blogpost
import qualified Demo.Distribution
import qualified Demo.Functions
import qualified Demo.HowToSpecifyIt
import qualified Demo.TestOptions
import qualified Demo.TestShrinking

main :: IO ()
main = defaultMain $ testGroup "demo" [
      Demo.TestOptions.tests
    , Demo.Functions.tests
    , Demo.TestShrinking.tests
    , Demo.Distribution.tests
    , Demo.HowToSpecifyIt.tests
    , Demo.Blogpost.tests
    ]
