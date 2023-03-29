module TestSuite.Prop.Generator.Compound (tests) where

import Test.Tasty
import Test.Tasty.Falsify

import Data.Falsify.List (Permutation)

import qualified Test.Falsify.Generator as Gen

-- TODO: We should verify that the suffix of a permutation is the identity.
tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Compound" [
      testGroup "Shrinking" [
        testProperty "perm" $
          testShrinking validPermShrink . gen $
            Gen.permutation 10
        ]
    ]

{-------------------------------------------------------------------------------
  Shuffling
-------------------------------------------------------------------------------}

validPermShrink :: Permutation -> Permutation -> Bool
validPermShrink perm1 perm2 = and [
      distance perm1 >= distance perm2
    , length perm1   >= length   perm2
    ]
  where
    distance :: Permutation -> Int
    distance = sum . map (\(i, j) -> abs (fromIntegral i - fromIntegral j))
