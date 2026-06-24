{-# LANGUAGE OverloadedStrings #-}

module TestSuite.Generator.Context (tests) where

import Test.Tasty
import Test.Tasty.Falsify

import qualified Data.Tree as Rose

import Test.Falsify
import Test.Falsify.SampleTree (Sample(..))

import qualified Test.Falsify.Context   as Context
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import Data.Bifunctor

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "TestSuite.Generator.Context" [
      testGroup "Sanity" [
          testProperty "toShrinkTree1" sanity_toShrinkTree1
        , testProperty "toShrinkTree2" sanity_toShrinkTree2
        ]
    ]

{-------------------------------------------------------------------------------
  'toShrinkTree'
-------------------------------------------------------------------------------}

sanity_toShrinkTree1 :: Property ()
sanity_toShrinkTree1 = do
    shrinkTree <- gen $ Gen.toShrinkTreeWithContext True (const g)
    assert $
      P.expect expected `P.dot` P.transparent hideNotShrunk
        .$ ("shrinkTree", shrinkTree)
  where
    g :: Gen Sample
    g = Gen.primWith (\case
          NotShrunk _ -> [0..2]
          Shrunk    0 -> []
          Shrunk    1 -> [0]
          Shrunk    x -> [0, x - 1]
        )

    expected :: ShrinkTree (Context.Execution, Sample)
    expected = WrapShrinkTree $
        Rose.Node (Context.Initial, NotShrunk 0) [
            Rose.Node (Context.Shrinking 0, Shrunk 0) [
                Rose.Node (Context.Final 1, Shrunk 0) []
              ]
          , Rose.Node (Context.Shrinking 0, Shrunk 1) [
                Rose.Node (Context.Shrinking 1, Shrunk 0) [
                    Rose.Node (Context.Final 2, Shrunk 0) []
                  ]
              ]
          , Rose.Node (Context.Shrinking 0, Shrunk 2) [
                Rose.Node (Context.Shrinking 1, Shrunk 0) [
                    Rose.Node (Context.Final 2, Shrunk 0) []
                  ]
              , Rose.Node (Context.Shrinking 1, Shrunk 1) [
                    Rose.Node (Context.Shrinking 2, Shrunk 0) [
                        Rose.Node (Context.Final 3,Shrunk 0) []
                      ]
                  ]
              ]
          ]

sanity_toShrinkTree2 :: Property ()
sanity_toShrinkTree2 = do
    shrinkTree <- gen $ Gen.toShrinkTreeWithContext True (const g)
    assert $
      P.expect expected `P.dot` P.transparent hideNotShrunk
        .$ ("shrinkTree", shrinkTree)
  where
    g :: Gen Sample
    g = Gen.primWith (const [])

    expected :: ShrinkTree (Context.Execution, Sample)
    expected = WrapShrinkTree $
        Rose.Node (Context.Initial, NotShrunk 0) [
            Rose.Node (Context.Final 0, NotShrunk 0) []
          ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | \"Hide\" unshrunk samples, so that we have deterministic output
hideNotShrunk :: ShrinkTree (ctxt, Sample) -> ShrinkTree (ctxt, Sample)
hideNotShrunk = fmap . second $ \case
    NotShrunk _ -> NotShrunk 0
    Shrunk    x -> Shrunk    x