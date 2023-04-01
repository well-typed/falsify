module TestSuite.Sanity.Compound (tests) where

import Data.Foldable (toList)
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Tree as Rose

import Data.Falsify.List (pairwiseAll)
import Data.Falsify.Tree (Tree(..))
import Test.Falsify.Debugging
import Test.Falsify.Generator (Gen, ShrinkTree)

import qualified Data.Falsify.Tree       as Tree
import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.Range      as Range
import qualified Test.Falsify.SampleTree as SampleTree

-- TODO: These tests should become property tests, using testMinimum and
-- testShrinking. Already migrated /some/ of these (see
-- TestSuite.Prop.Generator.Compound), but not yet all.

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Compound" [
      testGroup "tree" [
          testCase "towardsOrigin1"  test_tree_towardsOrigin1
        , testCase "towardsOrigin2"  test_tree_towardsOrigin2
        ]
    , testGroup "shrinkTree" [
          testCase "path"         test_shrinkTree_path
        , testCase "toShrinkTree" test_toShrinkTree
        ]
    ]

{-------------------------------------------------------------------------------
  Tree
-------------------------------------------------------------------------------}

test_tree_towardsOrigin1 :: Assertion
test_tree_towardsOrigin1 = do
    assertEqual "" expected $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 4)
  where
    -- If we prefer a counter-example with 10 nodes, then this tree is indeed
    -- nicely minimal: note that the tree is /almost/ balanced, it's off by
    -- the minimum amount that would make it unbalanced.
    expected :: Tree Word8
    expected =
        Branch 0
          (Branch 0
             Leaf
             (Branch 0
                (Branch 0 Leaf Leaf)
                (Branch 0 Leaf Leaf)
             )
          )
          (Branch 0
             (Branch 0 Leaf (Branch 0 Leaf Leaf))
             (Branch 0 Leaf (Branch 0 Leaf Leaf))
          )

    gen :: Gen (Tree Word8)
    gen = Gen.tree (Range.withOrigin (0, 100) 10) $
            Gen.integral $ Range.between (0, 1)

    prop :: Tree Word8 -> Bool
    prop = Tree.isHeightBalanced

test_tree_towardsOrigin2 :: Assertion
test_tree_towardsOrigin2 = do
    assertEqual "" expected $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 8)
  where
    -- Another beautiful example of a tree that is /not/ weight-balanced, but
    -- only barely so, with as side condition that we prefer to have 10 elements
    -- (and therefore /get/ 10 elements).
    expected :: Tree Word8
    expected =
        Branch 0
          (Branch 0
             (Branch 0 Leaf Leaf)
             (Branch 0 Leaf (Branch 0 Leaf Leaf))
          )
          -- This is the unbalanced subtree:
          (Branch 0
             Leaf
             (Branch 0
                (Branch 0 Leaf Leaf)
                (Branch 0 Leaf (Branch 0 Leaf Leaf))
             )
          )

    gen :: Gen (Tree Word8)
    gen = Gen.tree (Range.withOrigin (0, 100) 10) $
            Gen.integral $ Range.between (0, 1)

    prop :: Tree Word8 -> Bool
    prop = Tree.isWeightBalanced

{-------------------------------------------------------------------------------
  Rose trees
-------------------------------------------------------------------------------}

test_shrinkTree_path :: Assertion
test_shrinkTree_path = do
    assertEqual "" ["", "a", "aa"] $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 5)
  where
    gen :: Gen [String]
    gen = toList <$> Gen.pathAny st

    prop :: [String] -> Bool
    prop xs = length xs < 3

    -- Infinite ShrinkTree containing all strings containing lowercase letters
    st :: ShrinkTree String
    st = Rose.unfoldTree (\xs -> (xs, map (:xs) ['a' .. 'z'])) ""

test_toShrinkTree :: Assertion
test_toShrinkTree = do
    -- Should be any kind of path in which the last two pairs of numbers are
    -- NOT decreasing.
    assertEqual "" [16, 8, 54] $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 5)
  where
    genToTest :: Gen Word64
    genToTest = (`mod` 100) <$> Gen.prim

    gen :: Gen [Word64]
    gen = Gen.toShrinkTree genToTest >>= fmap toList . Gen.pathAny

    prop :: [Word64] -> Bool
    prop = pairwiseAll (>)
