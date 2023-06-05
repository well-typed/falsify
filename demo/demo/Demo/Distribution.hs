module Demo.Distribution (tests) where

import Data.List (intercalate)
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify.Range (Precision(..))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range

tests :: TestTree
tests = testGroup "Demo.Distribution" [
      testGroup "prim" [
          testProperty (show total) $ prop_prim total
        | total <- [2, 3, 6, 10]
        ]
    , testGroup "fraction" [
          testProperty (show p) $ prop_fraction p
        | p <- [2, 3, 4]
        ]
    , testGroup "inRange" [
          testProperty (show total) $ prop_integral total
        | total <- [2, 3, 6, 10]
        ]
    , testGroup "frequency" [
          testProperty (intercalate "_" $ [show a, show b, show c]) $
            prop_frequency a b c
        | (a, b, c) <- [(1, 1, 1), (1, 2, 3), (0, 1, 1), (1, 1, 8)]
        ]
    ]

prop_prim :: Word -> Property ()
prop_prim total = do
    x <- gen $ Gen.prim
    collect "bucket" [bucket bucketSize x]
  where
    bucketSize :: Word64
    bucketSize = maxBound `div` fromIntegral total

prop_fraction :: Precision -> Property ()
prop_fraction p = do
    x <- gen $ Gen.properFraction p
    collect "x" [x]

prop_integral :: Word -> Property ()
prop_integral total = do
    x <- gen $ Gen.inRange (Range.between (0, total - 1))
    collect "x" [x]

prop_frequency :: Word -> Word -> Word -> Property ()
prop_frequency a b c = do
    x <- gen $ Gen.frequency [
        (a, pure 'a')
      , (b, pure 'b')
      , (c, pure 'c')
      ]
    collect "x" [x]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

bucket :: forall a. (Ord a, Num a) => a -> a -> Word
bucket bucketSize = go 0
  where
    go :: Word -> a -> Word
    go b value
      | value <= bucketSize = b
      | otherwise           = go (succ b) (value - bucketSize)

