module TestSuite.Prop.Generator.Compound (tests) where

import Test.Tasty
import Test.Tasty.Falsify

import Data.Falsify.List (Permutation)
import Test.Falsify.Predicate (Predicate)

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

-- TODO: We should verify that the suffix of a permutation is the identity.
tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Compound" [
      testGroup "Shrinking" [
        testProperty "perm" $
          testShrinkingOfGen validPermShrink $
            Gen.permutation 10
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

