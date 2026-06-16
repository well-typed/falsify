{-# LANGUAGE OverloadedStrings #-}

module Demo.Context (tests) where

import Test.Tasty
import Test.Tasty.Falsify
import Data.Word

import Test.Falsify
import qualified Data.Falsify.ProperFraction as ProperFraction
import qualified Test.Falsify.Context        as Context
import qualified Test.Falsify.Generator      as Gen
import qualified Test.Falsify.Predicate      as P
import qualified Test.Falsify.Range          as Range

tests :: TestTree
tests = testGroup "Demo.Context" [
      testProperty "demo1"  demo1
    , testProperty "demo2a" demo2a
    , testProperty "demo2b" demo2b
    ]

{-------------------------------------------------------------------------------
  Show 'Context'
-------------------------------------------------------------------------------}

-- | Obviously false property
--
-- Run with
--
-- > cabal run -- demo -p Context --falsify-verbose
--
-- The point of this demo is simply to show how the execution 'Context' evolves.
demo1 :: Property ()
demo1 = do
    x    <- gen Gen.prim
    ctxt <- getContext
    info $ concat [
        "demo1: "
      , "generated " ++ show x
      , " in step " ++ show (Context.execution ctxt)
      ]
    assert $ P.expect 0 .$ ("x", x)

{-------------------------------------------------------------------------------
  Take advantage of the 'Context' to increase rangep
-------------------------------------------------------------------------------}

-- | "No number is equal to 5"
--
-- Obviously false, but hard to find a counter-example!
demo2a :: Property ()
demo2a = do
    x <- gen Gen.prim
    assert $ P.ne .$ ("forbidden", 5) .$ ("x", x :: Word64)

-- | .. but if we start with a small range and then increase, we always find it
demo2b :: Property ()
demo2b = do
    ctxt <- getContext
    l <- sized $ ProperFraction.scaleIntegral 100
    x <- gen $ Gen.inRange $ Range.inclusive (0, l)
    info (show (ctxt, l, x))
    assert $ P.ne .$ ("forbidden", 5) .$ ("x", x :: Word64)
