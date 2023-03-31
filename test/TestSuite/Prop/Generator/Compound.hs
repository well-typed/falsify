module TestSuite.Prop.Generator.Compound (tests) where

import Control.Monad
import Test.Tasty
import Test.Tasty.Falsify

import Data.Falsify.List (Permutation, applyPermutation)
import Test.Falsify.Predicate (Predicate)

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Compound" [
      testGroup "perm" [
          testProperty "shrinking" prop_perm_shrinking
        , testGroup "minimum" [
              testProperty (show n) $ prop_perm_minimum n
            | n <- [0 .. 9]
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Permutations (and shuffling)
-------------------------------------------------------------------------------}

validPermShrink :: Predicate [Permutation, Permutation]
validPermShrink = mconcat [
      P.ge `P.on` (P.fn "numSwaps" length  )
    , P.ge `P.on` (P.fn "distance" distance)
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
