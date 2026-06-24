{-# LANGUAGE OverloadedStrings #-}

module Demo.Context (tests) where

import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify
import qualified Test.Falsify.Context   as Context
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

tests :: TestTree
tests = testGroup "Demo.Context" [
      testProperty "demo1" demo1
    ]

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
