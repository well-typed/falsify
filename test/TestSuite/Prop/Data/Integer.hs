module TestSuite.Prop.Data.Integer (tests) where

import Data.List.NonEmpty (NonEmpty(..))
import Test.Tasty
import Test.Tasty.Falsify

import Data.Falsify.Integer
import Test.Falsify.Predicate ((.$))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range     as Range

tests :: TestTree
tests = testGroup "TestSuite.Prop.Data.Integer" [
      testProperty "toBits_fromBits"      prop_toBits_fromBits
    , testProperty "fromBits_toBits"      prop_fromBits_toBits
    , testProperty "dec_encEliasG"        prop_dec_encEliasG
    , testProperty "dec_encIntegerEliasG" prop_dec_encIntegerEliasG
    ]

prop_toBits_fromBits :: Property ()
prop_toBits_fromBits = do
    n <- gen $ fmap fromIntegral $
          Gen.integral $ Range.between (0 :: Word, 1_000_000)
    assert $
              P.expect n
      `P.dot` P.fn ("fromBits", natFromBits)
      `P.dot` P.fn ("toBits", natToBits)
           .$ ("n", n)

prop_fromBits_toBits :: Property ()
prop_fromBits_toBits = do
    bs <- gen $ Gen.list (Range.between (0, 20)) (Gen.elem (O :| [I]))
    assert $
              P.expect (dropLeadingZeroes bs)
      `P.dot` P.fn ("toBits", natToBits)
      `P.dot` P.fn ("fromBits", natFromBits)
           .$ ("bs", bs)
  where
    dropLeadingZeroes :: [Bit] -> [Bit]
    dropLeadingZeroes = dropWhile (== O)

prop_dec_encEliasG :: Property ()
prop_dec_encEliasG = do
    n    <- gen $ fmap fromIntegral $
             Gen.integral $ Range.between (1 :: Word, 1_000_000)
    junk <- gen $ Gen.list (Range.between (0, 20)) (Gen.elem (O :| [I]))
    assert $
              P.expect (Just (n, junk))
      `P.dot` P.fn ("decEliasG", decEliasG)
      `P.dot` P.fn ("withJunk", (++ junk))
      `P.dot` P.fn ("encEliasG", encEliasG)
           .$ ("n", n)

prop_dec_encIntegerEliasG :: Property ()
prop_dec_encIntegerEliasG = do
    n    <- gen $ fmap fromIntegral $
             Gen.integral $ Range.withOrigin (-1_000_000 :: Int, 1_000_000) 0
    collect ("n < 1") [n < 1]
    junk <- gen $ Gen.list (Range.between (0, 20)) (Gen.elem (O :| [I]))
    assert $
              P.expect (Just (n, junk))
      `P.dot` P.fn ("decEliasG", decIntegerEliasG)
      `P.dot` P.fn ("withJunk", (++ junk))
      `P.dot` P.fn ("encEliasG", encIntegerEliasG)
           .$ ("n", n)
