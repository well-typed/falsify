module TestSuite.Sanity.Tree (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify.Debugging
import Test.Falsify.Generator (Gen)

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.Range      as Range
import qualified Test.Falsify.SampleTree as SampleTree

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Tree" [
      testCase "full" test_full
    ]

-- | Generate a biased tree by generating a /full/ tree first
--
-- This is a demonstration that this approach is /not/ a good idea; see
-- comments inline, below.
test_full :: Assertion
test_full = do
    -- We can test by varying the depth of the tree that this scales /linearly/,
    -- which would not be the case if the full tree would be generated.
    let depth = 7

    -- /Generating/ the value (and testing it for bias) works linearly: the
    -- parts of the tree we never look at we never generate.
    assertBool "initial" $
      isBiased $ Gen.run (tree depth) (SampleTree.fromSeed 0)

   -- In /shrinking/ however we are not so lucky: we will be shrinking parts of
   -- the generated tree that the property does not depend on. Of course, the
   -- shortcut to introduce 'Minimal' would save us here, but that would defeat
   -- the purpose of the exercise: we want to leave those parts of the tree
   -- untouched /until/ we shrink some other values, so that we can shrink them
   -- independently. So this approach, where we generate a ton of values but
   -- then only look at a few of them, does not work.
    assertBool "shrunk" $
      all isBiased . shrinkHistory $
        shrinkExplain id ValidShrink (tree depth) (SampleTree.fromSeed 0)
  where
    tree :: Int -> Gen (Tree Word)
    tree 0 = pure Leaf
    tree d = do
        leftBiased <- Gen.bool False
        x     <- Gen.integral Range.full
        left  <- tree (d - 1)
        right <- tree (d - 1)
        return $
          if leftBiased
            then Branch x left Leaf
            else Branch x Leaf right

{-------------------------------------------------------------------------------
  Auxiliary: binary trees
-------------------------------------------------------------------------------}

data Tree a = Leaf | Branch a (Tree a) (Tree a)

isBiased :: Tree a -> Bool
isBiased Leaf                         = True
isBiased (Branch _ Leaf     t       ) = isBiased t
isBiased (Branch _ t        Leaf    ) = isBiased t
isBiased (Branch _ Branch{} Branch{}) = False

