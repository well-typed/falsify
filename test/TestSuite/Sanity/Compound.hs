module TestSuite.Sanity.Compound (tests) where

import Data.Foldable (toList)
import Data.List (nub)
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

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Compound" [
      testGroup "list" [
          testCase "towardsShorter"  test_list_towardsShorter
        , testCase "towardsLonger"   test_list_towardsLonger
        , testCase "towardsOrigin"   test_list_towardsOrigin
        ]
    , testGroup "tree" [
          testCase "towardsSmaller1" test_tree_towardsSmaller1
        , testCase "towardsSmaller2" test_tree_towardsSmaller2
        , testCase "towardsOrigin1"  test_tree_towardsOrigin1
        , testCase "towardsOrigin2"  test_tree_towardsOrigin2
        ]
    , testGroup "shrinkTree" [
          testCase "path"         test_shrinkTree_path
        , testCase "toShrinkTree" test_toShrinkTree
        ]
    ]

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

test_list_towardsShorter :: Assertion
test_list_towardsShorter = do
    -- [6, 4] is the minimal counter-example to a sorted list, when the elements
    -- are drawn from the range [0, 10] with origin 5, and filtered for even.
    --
    -- In principle the filtered list could /grow/ in size during shrinking (if
    -- a previously odd number is shrunk to be even).
    let expectedHistory = [
            [6,8,8,2,10,2,2]
          , [6,8,8,2,10,2]
          , [6,8,8,2]
          , [8,8,2]
          , [8,2]
          , [4,2]
          , [6,2]
          , [6,4]
          ]
     -- The 'nub' call here is still necessary; I guess because we are shrinking
     -- markers that we are not actually using? Might need to think about how
     -- to optimize that at some point, although this might be difficult:
     -- shrinking only values that are /used/ is hard.
    assertEqual "shrink" expectedHistory $
      nub $ shrink (not . prop) gen (SampleTree.fromSeed 1)
  where
    gen :: Gen [Word8]
    gen = fmap (filter even) $
            Gen.list (Range.between (10, 20)) $
              Gen.integral $ Range.linear (0, 10) 5

    prop :: [Word8] -> Bool
    prop = pairwiseAll (<=)

test_list_towardsLonger :: Assertion
test_list_towardsLonger = do
    -- We increase the list length to max immediately, and then shrink towards
    -- exactly one 1.
    let expectedHistory = [
            [1,1,1,0,0,0]
          , [1,1,1,0,0,0,0,0,1,1]
          , [0,1,1,0,0,0,0,0,1,1]
          , [0,0,1,0,0,0,0,0,1,1]
          , [0,0,1,0,0,0,0,0,0,0]
          ]
    assertEqual "shrink" expectedHistory $
      nub $ shrink (not . prop) gen (SampleTree.fromSeed 3)
  where
    gen :: Gen [Word8]
    gen = Gen.list (Range.between (10, 0)) $
            Gen.integral $ Range.between (0, 1)

    prop :: [Word8] -> Bool
    prop = pairwiseAll (<=)

test_list_towardsOrigin :: Assertion
test_list_towardsOrigin = do
    let expectedHistory = [
            [5,5,5,1]
          , [5,5,5,1,0]
          , [0,5,5,1,0]
          , [0,0,5,1,0]
          , [0,0,0,1,0]
          ]
    assertEqual "shrink" expectedHistory $
      nub $ shrink (not . prop) gen (SampleTree.fromSeed 14)
  where
    gen :: Gen [Word8]
    gen = Gen.list (Range.linear (0, 10) 5) $
            Gen.integral $ Range.between (0, 10)

    prop :: [Word8] -> Bool
    prop = pairwiseAll (<=)

{-------------------------------------------------------------------------------
  Tree
-------------------------------------------------------------------------------}

test_tree_towardsSmaller1 :: Assertion
test_tree_towardsSmaller1 = do
    assertEqual "" expected $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 0)
  where
    expected :: Tree Word8
    expected =
        Branch 0
          (Branch 0 Leaf Leaf)
          Leaf

    gen :: Gen (Tree Word8)
    gen = Gen.tree (Range.between (0, 100)) $
            Gen.integral $ Range.between (0, 1)

    prop :: Tree Word8 -> Bool
    prop = Tree.isHeightBalanced

test_tree_towardsSmaller2 :: Assertion
test_tree_towardsSmaller2 = do
    assertEqual "" expected $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 0)
  where
    -- For a minimal tree that is not weight-balanced, we need three elements in
    -- one subtree and none in the other: the weight of the empty tree is 1,
    -- the weight of the tree with three elements is 4, and 4 > Δ * 1, for Δ=3.
    expected :: Tree Word8
    expected =
        Branch 0
          (Branch 0
             Leaf
             (Branch 0
                Leaf
                (Branch 0 Leaf Leaf)
             )
          )
          Leaf

    gen :: Gen (Tree Word8)
    gen = Gen.tree (Range.between (0, 100)) $
            Gen.integral $ Range.between (0, 1)

    prop :: Tree Word8 -> Bool
    prop = Tree.isWeightBalanced

test_tree_towardsOrigin1 :: Assertion
test_tree_towardsOrigin1 = do
    assertEqual "" expected $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 0)
  where
    -- If we prefer a counter-example with 10 nodes, then this tree is indeed
    -- nicely minimal: note that the tree is /almost/ balanced, it's off by
    -- the minimum amount that would make it unbalanced.
    expected :: Tree Word8
    expected =
        Branch 0
          (Branch 0
             (Branch 0 Leaf Leaf)
             (Branch 0
                Leaf
                (Branch 0 Leaf Leaf))
          )
          (Branch 0
             (Branch 0
                Leaf
                (Branch 0 Leaf Leaf)
             )
             (Branch 0
                 (Branch 0 Leaf Leaf)
                 Leaf
             )
          )

    gen :: Gen (Tree Word8)
    gen = Gen.tree (Range.linear (0, 100) 10) $
            Gen.integral $ Range.between (0, 1)

    prop :: Tree Word8 -> Bool
    prop = Tree.isHeightBalanced

test_tree_towardsOrigin2 :: Assertion
test_tree_towardsOrigin2 = do
    assertEqual "" expected $
      last $ shrink (not . prop) gen (SampleTree.fromSeed 0)
  where
    -- Another beautiful example of a tree that is /not/ weight-balanced, but
    -- only barely so, with as side condition that we prefer to have 10 elements
    -- (and therefore /get/ 10 elements). The left subtree has one element,
    -- and therefore weight 2; the right subtree has 8 elements, and therefore
    -- weight 9, and 9 > Δ * 2 (for Δ=3). This tree indeed /just/ violates
    -- the weight-balancing property, whilst also having 10 elements.
    expected :: Tree Word8
    expected =
        Branch 0
           (Branch 0 Leaf Leaf)
           (Branch 0
              (Branch 0
                 (Branch 0 Leaf Leaf)
                 (Branch 0 Leaf Leaf)
              )
              (Branch 0
                 (Branch 0 Leaf Leaf)
                 (Branch 0
                    Leaf
                    (Branch 0 Leaf Leaf))
                 )
           )

    gen :: Gen (Tree Word8)
    gen = Gen.tree (Range.linear (0, 100) 10) $
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
