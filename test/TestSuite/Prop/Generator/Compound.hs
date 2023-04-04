module TestSuite.Prop.Generator.Compound (tests) where

import Control.Monad
import Data.Default
import Test.Tasty
import Test.Tasty.Falsify

import Data.Falsify.List (Permutation, applyPermutation, pairwiseAll)
import Data.Falsify.Tree (Tree(..))
import Test.Falsify.Predicate (Predicate, (.$))

import qualified Data.Falsify.Tree      as Tree
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range     as Range

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Compound" [
      testGroup "list" [
          testGroup "towardsShorter" [
              testProperty "shrinking" prop_list_towardsShorter_shrinking
            , testProperty "minimum"   prop_list_towardsShorter_minimum
            ]
        , testGroup "towardsShorterEven" [
              testPropertyWith expectFailure "shrinking" prop_list_towardsShorterEven_shrinking_wrong
            , testProperty                   "minimum"   prop_list_towardsShorterEven_minimum
            ]
        , testGroup "towardsLonger" [
              testProperty "shrinking" prop_list_towardsLonger_shrinking
            , testProperty "minimum"   prop_list_towardsLonger_minimum
            ]
        , testGroup "towardsOrigin" [
              testProperty "minimum" prop_list_towardsOrigin_minimum
            ]
        ]
    , testGroup "perm" [
          testProperty "shrinking" prop_perm_shrinking
        , testGroup "minimum" [
              testPropertyWith def{overrideMaxRatio = Just 1000}
                (show n) $ prop_perm_minimum n
            | n <- [0 .. 9]
            ]
        ]
    , testGroup "tree" [
          testProperty "towardsSmaller1" prop_tree_towardsSmaller1
        , testProperty "towardsSmaller2" prop_tree_towardsSmaller2
        ]
    , testGroup "frequency" [
          testProperty "shrinking" prop_frequency_shrinking
        , testPropertyWith expectFailure
            "shrinking_wrong" prop_frequency_shrinking_wrong
        , testProperty "replicateM" prop_replicateM_shrinking
        ]
    ]
  where
    expectFailure :: TestOptions
    expectFailure = def {
          expectFailure    = ExpectFailure
        , overrideNumTests = Just 10_000
        }

{-------------------------------------------------------------------------------
  Lists

  Here and elsewhere, for the 'testMinimum' tests, we don't /always/ fail, but
  check some property. This ensures that the minimum value isn't just always the
  one produced by the @Minimal@ sample tree.
-------------------------------------------------------------------------------}

prop_list_towardsShorter_shrinking :: Property ()
prop_list_towardsShorter_shrinking =
    testShrinkingOfGen (P.ge `P.on` P.fn ("length", length)) $
       Gen.list (Range.between (10, 20)) $
         Gen.int $ Range.between (0, 1)

prop_list_towardsShorter_minimum :: Property ()
prop_list_towardsShorter_minimum =
    testMinimum (P.satisfies ("expectedLength", (== 10) . length)) $ do
      xs <- gen $ Gen.list (Range.between (10, 20)) $
                    Gen.int $ Range.between (0, 1)
      unless (pairwiseAll (<=) xs) $ testFailed xs

-- In principle the filtered list can /grow/ in size during shrinking (if
-- a previously odd number is shrunk to be even).
prop_list_towardsShorterEven_shrinking_wrong :: Property ()
prop_list_towardsShorterEven_shrinking_wrong =
    testShrinkingOfGen (P.ge `P.on` P.fn ("length", length)) $
       fmap (filter even) $
         Gen.list (Range.between (10, 20)) $
           Gen.int $ Range.withOrigin (0, 10) 5

-- Although [6,4] is the perfect counter-example here, we don't always get it,
-- due to binary search
prop_list_towardsShorterEven_minimum :: Property ()
prop_list_towardsShorterEven_minimum =
    testMinimum (P.elem .$ ("expected", [[6,4],[4,2]])) $ do
      xs <- gen $ fmap (filter even) $
                     Gen.list (Range.between (10, 20)) $
                       Gen.int $ Range.withOrigin (0, 10) 5
      unless (pairwiseAll (<=) xs) $ testFailed xs

prop_list_towardsLonger_shrinking :: Property ()
prop_list_towardsLonger_shrinking =
    testShrinkingOfGen (P.le `P.on` P.fn ("length", length)) $
       Gen.list (Range.between (10, 0)) $
         Gen.int $ Range.between (0, 1)

prop_list_towardsLonger_minimum :: Property ()
prop_list_towardsLonger_minimum =
    testMinimum (P.satisfies ("expectedLength", (== 10) . length)) $ do
      xs <- gen $ Gen.list (Range.between (10, 0)) $
                    Gen.int $ Range.between (0, 1)
      unless (pairwiseAll (<=) xs) $ testFailed xs

prop_list_towardsOrigin_minimum :: Property ()
prop_list_towardsOrigin_minimum =
    testMinimum (P.satisfies ("expectedLength", (== 5) . length)) $ do
      xs <- gen $ Gen.list (Range.withOrigin (0, 10) 5) $
                    Gen.int $ Range.between (0, 1)
      unless (pairwiseAll (<=) xs) $ testFailed xs

{-------------------------------------------------------------------------------
  Permutations (and shuffling)
-------------------------------------------------------------------------------}

validPermShrink :: Predicate [Permutation, Permutation]
validPermShrink = mconcat [
      P.ge `P.on` P.fn ("numSwaps", length  )
    , P.ge `P.on` P.fn ("distance", distance)
    ]
  where
    distance :: Permutation -> Word
    distance = sum . map weighted

    weighted :: (Word, Word) -> Word
    weighted (i, j)
      | i < j     = error "unexpected swap"
      | otherwise = (10 ^ i) * (i - j)

prop_perm_shrinking :: Property ()
prop_perm_shrinking =
    testShrinkingOfGen validPermShrink $
       Gen.permutation 10

prop_perm_minimum :: Word -> Property ()
prop_perm_minimum n =
    testMinimum (P.satisfies ("suffixIsUnchanged", suffixIsUnchanged)) $ do
      perm <- gen $ Gen.permutation 10
      let shuffled = applyPermutation perm [0 .. 9]
      when (shuffled !! fromIntegral n /= n) $ testFailed perm
  where
    suffixIsUnchanged :: Permutation -> Bool
    suffixIsUnchanged perm =
        case perm of
          [(i, j)]   -> i == j + 1 && (i == n || j == n)
          _otherwise -> False

{-------------------------------------------------------------------------------
  Tree

  TODO: We're currently only testing minimums here.
-------------------------------------------------------------------------------}

prop_tree_towardsSmaller1 :: Property ()
prop_tree_towardsSmaller1 =
    testMinimum (P.expect expected) $ do
      t <- gen $ Gen.tree (Range.between (0, 100)) $
                   Gen.int $ Range.between (0, 1)
      -- "Every tree is height balanced"
      unless (Tree.isHeightBalanced t) $ testFailed t
  where
    expected :: Tree Int
    expected = Branch 0 Leaf (Branch 0 Leaf (Branch 0 Leaf Leaf))

prop_tree_towardsSmaller2 :: Property ()
prop_tree_towardsSmaller2 =
    testMinimum (P.elem .$ ("expected", expected)) $ do
      t <- gen $ Gen.tree (Range.between (0, 100)) $
                   Gen.int $ Range.between (0, 1)
      -- "Every tree is weight balanced"
      unless (Tree.isWeightBalanced t) $ testFailed t
  where
    -- For a minimal tree that is not weight-balanced, we need three elements in
    -- one subtree and none in the other: the weight of the empty tree is 1,
    -- the weight of the tree with three elements is 4, and 4 > Δ * 1, for Δ=3.
    expected :: [Tree Int]
    expected = [
          Branch 0 (Branch 0 (Branch 0 Leaf Leaf) (Branch 0 Leaf Leaf)) Leaf
        , Branch 0 (Branch 0 Leaf (Branch 0 Leaf (Branch 0 Leaf Leaf))) Leaf
        , Branch 0 Leaf (Branch 0 (Branch 0 Leaf Leaf) (Branch 0 Leaf Leaf))
        , Branch 0 Leaf (Branch 0 Leaf (Branch 0 Leaf (Branch 0 Leaf Leaf)))
        ]

{-------------------------------------------------------------------------------
  Tweak test data distribution
-------------------------------------------------------------------------------}

propShrinkingList1 :: [Word] -> [Word] -> Bool
propShrinkingList1 = aux
  where
    aux [_, _, _] [_, _]       = True
    aux [_, _, _] [_]          = True
    aux [_, _]    [_]          = True
    aux [x]       [x']         = x >= x'
    aux [x, y]    [x', y']     = x >= x' && y >= y'
    aux [x, y, z] [x', y', z'] = x >= x' && y >= y' && z >= z'
    aux _         _            = error "impossible"

propShrinkingList2 :: [Word] -> [Word] -> Bool
propShrinkingList2 = aux
  where
    aux :: [Word] -> [Word] -> Bool
    aux [x, y, _] [x', y']     = x >= x' && y >= y'
    aux [x, _, _] [x']         = x >= x'
    aux [x, _]    [x']         = x >= x'
    aux [x]       [x']         = x >= x'
    aux [x, y]    [x', y']     = x >= x' && y >= y'
    aux [x, y, z] [x', y', z'] = x >= x' && y >= y' && z >= z'
    aux _         _            = error "impossible"

genListFrequency :: Gen [Word]
genListFrequency =
    Gen.frequency [
        (1, replicateM 1 $ Gen.integral $ Range.between (0, 10))
      , (2, replicateM 2 $ Gen.integral $ Range.between (0, 10))
      , (3, replicateM 3 $ Gen.integral $ Range.between (0, 10))
      ]

genListMonad :: Gen [Word]
genListMonad = do
    n <- Gen.integral $ Range.between (1, 3)
    replicateM n $ Gen.integral $ Range.between (0, 10)

prop_frequency_shrinking :: Property ()
prop_frequency_shrinking =
    testShrinkingOfGen
      (P.relatedBy ("propShrinkingList1", propShrinkingList1))
      genListFrequency

-- 'propShrinkingList2' does /not/ hold for 'genListFrequency' because the
-- generators are independent
prop_frequency_shrinking_wrong :: Property ()
prop_frequency_shrinking_wrong =
    testShrinkingOfGen
      (P.relatedBy ("propShrinkingList2", propShrinkingList2))
      genListFrequency

-- 'propShrinkingList2' /does/ hold if we simply use 'replicateM'.
prop_replicateM_shrinking :: Property ()
prop_replicateM_shrinking =
    testShrinkingOfGen
      (P.relatedBy ("propShrinkingList2", propShrinkingList2))
      genListMonad
