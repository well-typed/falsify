module Main (main) where

import Test.Tasty

import qualified Demo.Blogpost
import qualified Demo.Distribution
import qualified Demo.Functions
import qualified Demo.HowToSpecifyIt
import qualified Demo.TestOptions
import qualified Demo.TestShrinking
import qualified Test.Falsify.Generator as Gen
import Test.Falsify.Property (gen)
import qualified Test.Falsify.Range as Range
import Control.Exception (throwIO)
import Control.Monad (guard, when)
import Test.Tasty.Falsify (testPropertyIO)

main :: IO ()
main = defaultMain $ testGroup "demo" [
    testPropertyIO "foo" $ do
        x <- gen $ Gen.int $ Range.between (0, 500)
        return $
            when (22 < x && x < 40) $ throwIO (userError "aaaAAAaaAAAa")
    --   Demo.TestOptions.tests
    -- , Demo.Functions.tests
    -- , Demo.TestShrinking.tests
    -- , Demo.Distribution.tests
    -- , Demo.HowToSpecifyIt.tests
    -- , Demo.Blogpost.tests
    ]
