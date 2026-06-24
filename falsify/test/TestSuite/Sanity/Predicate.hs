{-# LANGUAGE OverloadedStrings #-}

module TestSuite.Sanity.Predicate (tests) where

import Data.Char
import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify
import qualified Test.Falsify.Predicate as P

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Predicate" [
      testCase "on" test_on
    ]

{-------------------------------------------------------------------------------
  Test: 'P.on'
-------------------------------------------------------------------------------}

test_on :: Assertion
test_on = do
    assertEqual "ok"   (Right ())  $ P.eval $ p1 .$ ("x", 'a') .$ ("y", 'a')
    assertEqual "err1" (Left err1) $ P.eval $ p1 .$ ("x", 'a') .$ ("y", 'b')
    assertEqual "err2" (Left err2) $ P.eval $ p2 .$ ("x", 'a') .$ ("y", 'b')
  where
    p1, p2 :: Predicate '[Char, Char]
    p1 = P.eq `P.on` P.fn ("ord", ord)
    p2 = P.eq `P.on` P.transparent ord

    err1, err2 :: String
    err1 = unlines [
               "(ord x) /= (ord y)"
             , "x    : 'a'"
             , "y    : 'b'"
             , "ord x: 97"
             , "ord y: 98"
             ]
    err2 = unlines [
               "x /= y"
             , "x: 'a'"
             , "y: 'b'"
             ]