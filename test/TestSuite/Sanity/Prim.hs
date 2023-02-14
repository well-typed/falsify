module TestSuite.Sanity.Prim (tests) where

import Control.Monad
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as Set

import Data.Falsify.List (pairwiseAll, pairwiseAny)
import Test.Falsify.Debugging
import Test.Falsify.Generator (Gen)
import Test.Falsify.SampleTree (SampleTree, Sample (..))

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.SampleTree as SampleTree

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Prim" [
      testCase "pair_word_word"       test_pair_word_word
    , testCase "list_allEqual"        test_list_allEqual
    , testCase "list_sorted"          test_list_sorted
    , testCase "maybe_towardsNothing" test_maybe_towardsNothing
    , testCase "maybe_towardsJust"    test_maybe_towardsJust
    , testCase "either"               test_either
    , testCase "stream"               test_stream
    ]

test_pair_word_word :: Assertion
test_pair_word_word = do
    assertEqual "run-3-2" (3, 2) $
      Gen.run gen $ tree 3 2
    assertEqual "run-2-3" (2, 3) $
      Gen.run gen $ tree 2 3

    let expectedkHistory32 = (3, 2) :| [
            (2, 2)
          , (2, 0)
          , (1, 0)
          ]
    assertEqual "shrink-3-2" expectedkHistory32 $
      Gen.shrink (not . prop1) gen (tree 3 2)

    let expectedHistory23 = (2, 3) :| [
            (1, 3)
          , (1, 2)
          , (1, 1)
          ]
    assertEqual "shrink-2-3" expectedHistory23 $
      Gen.shrink (not . prop2) gen (tree 2 3)
  where
    gen :: Gen (Word64, Word64)
    gen = (,) <$> Gen.prim <*> Gen.prim

    tree :: Word64 -> Word64 -> SampleTree
    tree x y = expandTruncated $ B (S (NotShrunk x)) (B (S (NotShrunk y)) E)

    -- for all pairs (x, y), x < y
    prop1 :: (Word64, Word64) -> Bool
    prop1 (x, y) = x == 0 || x < y

    -- for all pairs (x, y), x > y
    prop2 :: (Word64, Word64) -> Bool
    prop2 (x, y) = x == 0 || x > y

test_list_allEqual :: Assertion
test_list_allEqual = do
    assertEqual "run" [1, 1, 2] $
      Gen.run gen $ tree 1 1 2

    let expectedHistory = [1,1,2] :| [[0,1,2],[0,1]]
    assertEqual "shrink" expectedHistory $
      Gen.shrink (not . prop) gen (tree 1 1 2)
  where
    -- Tree that produces gen of three elements in the specified order
    tree :: Word64 -> Word64 -> Word64 -> SampleTree
    tree x y z = expandTruncated $
        B (S (NotShrunk 3))
          (B (S (NotShrunk x)) (B
          (B (S (NotShrunk y)) (B
          (B (S (NotShrunk z)) (B E
            E)) E)) E))

    gen :: Gen [Word64]
    gen = do
        n <- Gen.prim
        replicateM (fromIntegral n) Gen.prim

    -- all elements are equal
    prop :: [Word64] -> Bool
    prop = pairwiseAll (==)

test_list_sorted :: Assertion
test_list_sorted = do
    -- This shows how we can write a generator that can drop arbitrary elements.
    -- Unlike Hypothesis, as we drop elements from the list, as long as the
    -- structure of the generator does not change, we don't re-interpret samples
    -- in a different context. We get HDD for free.
    -- (This generator can easily be combined with picking a list length first.)
    let expectedHistory = [1,3,2] :| [[3,2],[3,0],[2,0],[1,0]]
    assertEqual "not . prop" expectedHistory $
      Gen.shrink (not . prop) (gen 3) (tree 1 3 2)
  where
    -- Tree that produces list of three elements in the specified order
    tree :: Word64 -> Word64 -> Word64 -> SampleTree
    tree x y z = expandTruncated $
        B (B (S (NotShrunk 1)) (B (S (NotShrunk x)) E)) (B (
        B (B (S (NotShrunk 1)) (B (S (NotShrunk y)) E)) (B (
        B (B (S (NotShrunk 1)) (B (S (NotShrunk z)) E)) (B E
        E)) E)) E)

    gen :: Int -> Gen [Word64]
    gen n = catMaybes <$> replicateM n (aux <$> Gen.prim <*> Gen.prim)
      where
        aux :: Word64 -> Word64 -> Maybe Word64
        aux 0 = const Nothing
        aux _ = Just

    -- the list is sorted
    prop :: [Word64] -> Bool
    prop = pairwiseAll (<=)

test_maybe_towardsNothing :: Assertion
test_maybe_towardsNothing = do
    -- When we start with the minimal tree, we get 'Nothing'
    let minimalTree = expandTruncated E
        (nothingTree, nothingResult) = (
            B (S (Shrunk 0)) (S (Shrunk 0))
          , (Nothing, 0)
          )
    assertEqual "run Nothing" (nothingTree, nothingResult) $
      runExplain gen minimalTree

    -- To find out the shape of the tree required by the generator to produce
    -- 'Just', we need to replace the first @0@ by a @1@:
    let modifiedTree =
          let pick :: [Sample] -> Sample
              pick ss = error $ "pick: unexpected " ++ show ss
          in expandTruncated' (pick . Set.toList)  . toTruncated' $
               replaceValues [NotShrunk 1] nothingTree
        (justTree, justResult) = (
            B (S (NotShrunk 1)) (B (S (Shrunk 0)) (B (S (Shrunk 0)) E))
          , (Just 0, 0)
          )
    assertEqual "run Just" (justTree, justResult) $
      runExplain gen modifiedTree

    -- When we merge these two trees, we realize that the generators look
    -- at different parts of the sample tree (other than that first value)
    let mergedTree :: Word64 -> Word64 -> Word64 -> Truncated'
        mergedTree x y z =
          B' (S' (Set.fromList [Shrunk 0, NotShrunk 1]))
             (F' (Set.fromList [Shrunk x])
                 (S' (Set.fromList [Shrunk y])) (B'
                 (S' (Set.fromList [Shrunk z])) E'
                 ))
    assertEqual "merged" (mergedTree 0 0 0) $
         toTruncated' nothingTree <> toTruncated' justTree

    let tree =
          let pick :: [Sample] -> Sample
              pick [NotShrunk 1, Shrunk 0] = NotShrunk 1
              pick ss = error $ "pick: unexpected " ++ show ss
          in expandTruncated' (pick . Set.toList) $ mergedTree 5 1 3
        expectedHistoryNotProp = (Just 1, 3) :| [
            (Nothing, 5)
          , (Nothing, 3)
          ]
    assertEqual "not . prop" expectedHistoryNotProp $
      Gen.shrink (not . prop) gen tree
  where
    gen :: Gen (Maybe Word64, Word64)
    gen = do
        genNothing <- (== 0) <$> Gen.prim
        if genNothing
          then (\  y -> (Nothing, y)) <$>              Gen.prim
          else (\x y -> (Just x,  y)) <$> Gen.prim <*> Gen.prim

    prop :: (Maybe Word64, Word64) -> Bool
    prop (_, y) = even y

test_maybe_towardsJust :: Assertion
test_maybe_towardsJust = do
    -- Unlike hypothesis, we are always dealing with infinite sample tree; if a
    -- "simpler" test case needs more samples, then they are available.
    -- (Note how we are shrinking from Nothing to Just in this example).
    let expectedHistory = (Nothing,7) :| [(Just 6,2),(Just 0,2),(Just 0,1)]
    assertEqual "const True" expectedHistory $
      Gen.shrink (not . prop) gen (SampleTree.mod 10 $ SampleTree.fromSeed 0)
  where
    gen :: Gen (Maybe Word64, Word64)
    gen = do
        genJust <- (== 0) <$> Gen.prim
        if genJust
          then (\x y -> (Just x,  y)) <$> Gen.prim <*> Gen.prim
          else (\  y -> (Nothing, y)) <$>              Gen.prim

    prop :: (Maybe Word64, Word64) -> Bool
    prop (_, x) = x == 0

test_either :: Assertion
test_either = do
    -- Like Hypothesis, when the structure of the generator changes, we might
    -- reinterpret parts of the sample tree in a different context.
    let expectedHistory1 = Right 4 :| [
            Right 2
          , Left  2  -- we are reusing the shrunk 2 as the 'Left' argument
          , Left  1
          ]
    assertEqual "gen1" expectedHistory1 $
      Gen.shrink (not . prop) gen1 (tree1 4)

    -- We can avoid this however by always generating both branches.
    let expectedHistory2 = Right 4 :| [
            Right 4
          , Right 2
            -- Shrinking to Left at this point would result in Left 4,
            -- which is not a counter-example.
          ]
    assertEqual "gen2" expectedHistory2 $
      Gen.shrink (not . prop) gen2 (tree2 4)

    -- TODO: Might this is in fact OK to do for Either, /by default/..? Is
    -- everything is lazy enough that only the parts we look at are generated,
    -- and as we shrink, the other parts should be GCed as we go? Unsure. Need
    -- to think carefully/experiment.
  where
    gen1 :: Gen (Either Word64 Word64)
    gen1 = do
        genLeft <- (== 0) <$> Gen.prim -- shrink towards left
        if genLeft
          then Left  <$> Gen.prim
          else Right <$> Gen.prim

    tree1 :: Word64 -> SampleTree
    tree1 x = expandTruncated $ B (S (NotShrunk 1)) (S (NotShrunk x))

    prop :: Either Word64 Word64 -> Bool
    prop (Right y) = odd y  || y == 0
    prop (Left  x) = x == 4 || x == 0

    gen2 :: Gen (Either Word64 Word64)
    gen2 = aux <$> Gen.prim <*> Gen.prim <*> Gen.prim
      where
        aux :: Word64 -> Word64 -> Word64 -> Either Word64 Word64
        aux 0 x _ = Left  x
        aux _ _ y = Right y

    tree2 :: Word64 -> SampleTree
    tree2 x = expandTruncated $
        B (B (S (NotShrunk 1)) (B (S (NotShrunk x)) E)) (B (S (NotShrunk x)) E)

test_stream :: Assertion
test_stream = do
    assertEqual "run" [3,7,5,1,3,2,1] $
      uncurry prefix $ Gen.run gen $ SampleTree.mod 10 (SampleTree.fromSeed 1)

    let expectedHistory = [3,7,5,1,3,2,1] :| [
            [2,7,5,1,3,2,1]
          , [1,7,5,1,3,2,1]
          , [1,4,5,1,3,2,1]
          , [1,2,5,1,3,2,1]
          , [1,2,3,1,3,2,1]
          , [1,2,3,1,3,2,1]
          , [1,2,3,1]
          , [1,2,3,1]
          ]
    assertEqual "shrink" expectedHistory $
      fmap (uncurry prefix) $
        Gen.shrink (not . prop) gen (SampleTree.mod 10 $ SampleTree.fromSeed 1)
  where
    gen :: Gen (Stream Word64, Word64)
    gen = (,) <$> genStream <*> Gen.prim

    genStream :: Gen (Stream Word64)
    genStream = Stream <$> Gen.prim <*> genStream

    prop :: (Stream Word64, Word64) -> Bool
    prop = aux . uncurry prefix
      where
        aux :: [Word64] -> Bool
        aux xs = pairwiseAll (\x y -> y == succ x) xs
              || pairwiseAny (==)                  xs
              || any (== 0)                        xs

{-------------------------------------------------------------------------------
  Auxiliary: infinite streams
-------------------------------------------------------------------------------}

-- Infinite streams
data Stream a = Stream a (Stream a)

prefix :: Stream a -> Word64 -> [a]
prefix _             0 = []
prefix (Stream x xs) n = x : prefix xs (pred n)




