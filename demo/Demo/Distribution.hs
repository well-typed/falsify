module Demo.Distribution (tests) where

import Data.List (intercalate)
import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify.Generator (Precision(..))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range

tests :: TestTree
tests = testGroup "Demo.Distribution" [
      testGroup "prim" [
          testProperty (show total) $ prop_prim total
        | total <- [2, 3, 6, 10]
        ]
    , testGroup "fraction" [
          testProperty (intercalate "_" $ [show p, show total]) $
            prop_fraction (Precision p) total
        | p <- [2, 3, 4]
        , total <- [2, 3, 6, 10]
        ]
    , testGroup "integral" [
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
    collect "x" [x `div` (maxBound `div` fromIntegral total)]

prop_fraction :: Precision -> Word -> Property ()
prop_fraction p total = do
    x <- gen $ Gen.fraction p
    collect "x" [Gen.getFraction $ x / (maxBound / fromIntegral total)]

prop_integral :: Word -> Property ()
prop_integral total = do
    x <- gen $ Gen.integral (Range.between (0, total - 1))
    collect "x" [x]

prop_frequency :: Word -> Word -> Word -> Property ()
prop_frequency a b c = do
    x <- gen $ Gen.frequency [
        (a, pure 'a')
      , (b, pure 'b')
      , (c, pure 'c')
      ]
    collect "x" [x]


