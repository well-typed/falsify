module TestSuite.Sanity.Selective (tests) where

import Control.Selective
import Data.Word
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify.Generator (Gen, Tree(..))
import Test.Falsify.Interactive (sample, shrink')

import qualified Test.Falsify.Generator as Gen

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Selective" [
      testGroup "tree" [
          testCaseInfo "ifBoth" test_tree_ifBoth
        , testGroup "ifS" [
              testCase  "10" $ test_tree_ifS  10
            , testCase  "20" $ test_tree_ifS  20
            , testCase  "30" $ test_tree_ifS  30
            , testCase  "40" $ test_tree_ifS  40
            , testCase  "50" $ test_tree_ifS  50
            , testCase  "60" $ test_tree_ifS  60
            , testCase  "70" $ test_tree_ifS  70
            , testCase  "80" $ test_tree_ifS  80
            , testCase  "90" $ test_tree_ifS  90
            , testCase "100" $ test_tree_ifS 100
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Tree

  In this test we construct a "biased tree" (aka list) using a generator for a
  /complete/ tree but then only using part of the result. Clearly, if we
  /actually/ used the entire complete tree, this would have exponential
  complexity, so that's not an option.

  The problem is not in /generation/, which is sufficiently lazy, but in
  shrinking. With the monadic interface, there are two non-solutions:

  - With the shrinking shortcut in place (reducing entire prats of the tree
    to 'Minimal'), then shrinking isn't all that interesting: the part of the
    tree we're not using will be set to all zeroes immediately (this is what
    the @either@ examples were demonstrating)
  - Without the shrinking shortcut in place, the /generator/ might not look
    at the full complete tree, but the /shrinker/ will, and so shrinking will
    have abysmal performance. This is demonstrated in 'test_tree_ifBoth'.

  With the selective interface, however, everything works just fine.
-------------------------------------------------------------------------------}

test_tree_ifBoth :: IO String
test_tree_ifBoth = do
    let depth = 15
    -- Verify that we /don't/ get a timeout during generation
    sampled <- sample (tree ifBoth depth)
    assertBool "initial" $ isBiased sampled
    -- But we /do/ get a timeout during shrinking
    didTimeout <- timeout 10_000_000 $ do
      Just history <- shrink' Just (tree ifBoth depth)
      assertBool "shrunk" $ all isBiased history
      return history
    case didTimeout of
      Nothing      -> return "Timed out as expected"
      Just history -> assertFailure $ unlines [
          "Expected timeout, but did not get it. "
        , "Shrink history: " ++ show history
        ]

test_tree_ifS :: Word64 -> Assertion
test_tree_ifS depth = do
    sampled <- sample (tree ifS depth)
    assertBool "initial" $ isBiased sampled
    Just shrunk <- shrink' Just (tree ifS depth)
    assertBool "shrunk" $ all isBiased shrunk

isBiased :: Tree a -> Bool
isBiased Leaf                         = True
isBiased (Branch _ Leaf     t       ) = isBiased t
isBiased (Branch _ t        Leaf    ) = isBiased t
isBiased (Branch _ Branch{} Branch{}) = False

tree ::
     (forall a. Gen Bool -> Gen a -> Gen a -> Gen a)
  -> Word64 -> Gen (Tree Word64)
tree if_ = go
  where
    go :: Word64 -> Gen (Tree Word64)
    go 0 = pure Leaf
    go d =
        Gen.prim `Gen.bindWithoutShortcut` \x ->
        if_ ((== 0) <$> Gen.prim)
            ((\t -> Branch x t Leaf) <$> go (d - 1))
            ((\t -> Branch x Leaf t) <$> go (d - 1))

{-------------------------------------------------------------------------------
  Generic auxiliary
-------------------------------------------------------------------------------}

ifBoth :: Gen Bool -> Gen a -> Gen a -> Gen a
ifBoth cond t f =
    t `Gen.bindWithoutShortcut` \x ->
    f `Gen.bindWithoutShortcut` \y ->
    cond `Gen.bindWithoutShortcut` \b  ->
    return $ if b then x else y