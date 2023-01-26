module TestSuite.Sanity.Generator (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Word

import Test.Falsify.Entropy (Entropy(..))
import Test.Falsify.Entropy.Tree (EntropyTree(L, B))
import Test.Falsify.Generator (Gen)

import qualified Test.Falsify.Generator as Gen

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Generator" [
      testCase "foo" test_foo
    ]

test_foo :: Assertion
test_foo = do
    let r :: Gen.Result (Maybe (Word8, Word8), Word8)
        r = Gen.run generator (Entropy [128, 1, 2, 3])
    print r

    print $ Gen.rerun generator tree0
    print $ Gen.rerun generator tree1
    print $ Gen.rerun generator tree2
    print $ Gen.rerun generator tree3
  where
    generator :: Gen (Maybe (Word8, Word8), Word8)
    generator = (,) <$> x <*> Gen.byte
      where
        x :: Gen (Maybe (Word8, Word8))
        x = do
            isJust <- (>= 128) <$> Gen.byte
            if isJust
              then Just <$> ((,) <$> Gen.byte <*> Gen.byte)
              else return Nothing

    tree0 :: EntropyTree
    tree0 =
        B (B (L [128])
             (B (L [1]) (L [2]))
          )
          (L [3])

    tree1 :: EntropyTree
    tree1 =
        B (B (L [0])
             (B (L [1]) (L [2]))
          )
          (L [3])

    tree2 :: EntropyTree
    tree2 =
        B
          (B (L [0])
             (L [])
          )
          (L [3])

    tree3 :: EntropyTree
    tree3 =
        B
          (B (L [])
             (L [])
          )
          (L [])
