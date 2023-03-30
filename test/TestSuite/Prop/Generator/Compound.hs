module TestSuite.Prop.Generator.Compound (tests) where

import Control.Monad
import Test.Tasty
import Test.Tasty.Falsify

import Data.Falsify.List (Permutation)
import Test.Falsify.Predicate (Predicate)

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Compound" [
      testProperty "perm_shrinking"  prop_perm_shrinking
    , testGroup "shuffle_minimum" [
          testProperty (show n) $ prop_shuffle_minimum n
        | n <- [0 .. 9]
        ]
    ]

{-------------------------------------------------------------------------------
  Shuffling
-------------------------------------------------------------------------------}

validPermShrink :: Predicate [Permutation, Permutation]
validPermShrink = mconcat [
      P.ge `P.on` (P.fn "numSwaps" length  )
    , P.ge `P.on` (P.fn "distance" distance)
    ]
  where
    distance :: Permutation -> Word
    distance = sum . map between

    between :: (Word, Word) -> Word
    between (i, j)
      | i < j     = j - i
      | otherwise = i - j

prop_perm_shrinking :: Property ()
prop_perm_shrinking =
    testShrinkingOfGen validPermShrink $
       Gen.permutation 10

prop_shuffle_minimum :: Int -> Property ()
prop_shuffle_minimum n =
    testMinimum (P.satisfies ("suffixIsUnchanged", suffixIsUnchanged)) $ do
      perm <- gen $ Gen.shuffle [0 .. 9]
      when (perm !! n /= n) $ testFailed perm
  where
    suffixIsUnchanged :: [Int] -> Bool
    suffixIsUnchanged perm = drop n perm == drop n [0 .. 9]


