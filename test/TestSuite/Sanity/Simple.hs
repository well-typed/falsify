module TestSuite.Sanity.Simple (tests) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Test.Falsify.Debugging
import Test.Falsify.Generator (Gen)
import Test.Falsify.SampleTree (SampleTree, Sample (..))

import qualified Test.Falsify.Generator  as Gen
import qualified Test.Falsify.Range      as Range
import qualified Test.Falsify.SampleTree as SampleTree

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Simple" [
      testCase "bool"     test_bool
    , testCase "integral" test_integral
    ]

test_bool :: Assertion
test_bool = do
    assertEqual "run" (False, True) $
      Gen.run gen SampleTree.minimal

    let expectedHistory = (True,False) :| [
            (False,True)
          ]
    assertEqual "shrink" expectedHistory $
      Gen.shrink (const True) gen (tree maxBound maxBound)
  where
    gen :: Gen (Bool, Bool)
    gen = (,) <$> Gen.bool False <*> Gen.bool True

    tree :: Word64 -> Word64 -> SampleTree
    tree x y = expandTruncated $
        B (S (NotShrunk x)) (B (S (NotShrunk y)) E)

test_integral :: Assertion
test_integral = do
    assertEqual "run minBound" (0, 6, 3) $
      Gen.run gen $ tree minBound

    -- @maxBound@ corresponds in the maximum /negative/ fraction.
    -- This corresponds to being as far /left/ of the origin as possible
    -- when the range is split; otherwise, it simply corresponds to being
    -- as far away as possible from the origin.
    assertEqual "run maxBound" (6, 0, 0) $
      Gen.run gen $ tree maxBound

    -- @maxBound - 1@ corresponds in the maximum /positive/ fraction.
    -- This corresponds to being as far /right/ of the origin as possible if
    -- split; otherwise, the sign makes no difference.
    assertEqual "run maxBound-1" (6, 0, 6) $
      Gen.run gen $ tree (maxBound - 1)

    -- Note that we are shrinking to a perfect minimal counter-example here: the
    -- 6 and 3 are at their "minimal" value, and if we were to shrink that 1 any
    -- further, it would no longer be a counter-example.
    let expectedHistory = (6,0,6) :| [
            (3,0,6)
          , (2,0,6)
          , (1,0,6)
          , (1,0,6)
          , (1,6,6)
          , (1,6,3)
          ]
    assertEqual "shrink" expectedHistory $
      Gen.shrink (not . prop) gen (tree (maxBound - 1))
  where
    gen :: Gen (Word, Word, Word)
    gen = (,,)
        <$> Gen.integral (Range.num (0, 6) 0)
        <*> Gen.integral (Range.num (0, 6) 6)
        <*> Gen.integral (Range.num (0, 6) 3)

    tree :: Word64 -> SampleTree
    tree x = expandTruncated $
        B (B (S (NotShrunk x)) (B (S (NotShrunk x)) E)) (B (S (NotShrunk x)) E)

    prop :: (Word, Word, Word) -> Bool
    prop (x, y, z) = (x == 0) || odd x && odd y && odd z
