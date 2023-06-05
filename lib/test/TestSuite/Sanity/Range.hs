module TestSuite.Sanity.Range (tests) where

import Control.Monad
import Data.Bifunctor
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import qualified Data.Map as Map

import Test.Falsify.Range (Range, Precision(..), ProperFraction(..))

import qualified Test.Falsify.Range as Range

tests :: TestTree
tests = testGroup "TestSuite.Sanity.Range" [
      testGroup "between" [
          testCase (show size) $ test_between size
        | size <- [2, 3, 4, 10, 100, 1000, 10_000]
        ]
    ]

test_between :: Word -> Assertion
test_between size = do
     assertEqual "domain" [0 .. size - 1] $
       map fst $ stats r

     forM_ (map snd $ stats r) $ \(Percentage pct _) ->
       unless (abs (pct - expected) < tolerance) $
         assertFailure $ concat [
             "Percentage "
           , show pct
           , ". Expected "
           , show expected
           , " (tolerance "
           , show tolerance
           , ")"
           ]
   where
     r :: Range Word
     r = Range.between (0, size - 1)

     expected, tolerance :: Double
     expected  = 1 / fromIntegral size
     tolerance = 0.01

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data Percentage = Percentage Double Bool

instance Show Percentage where
  show (Percentage pct isZero) =
      printf "%8.4f%% (%s)" pct (if isZero then "zero" else "non-zero")

-- | Compute statistics about the given range
--
-- Whenever the 'Range' asks for a fraction with a certain precision, we give
-- it /all/ possible fractions with that precision. We then count how often
-- each value in the range is produced.
stats :: forall a. Ord a => Range a -> [(a, Percentage)]
stats r =
    count Map.empty $ Range.eval genFraction r
  where
    genFraction :: Precision -> [ProperFraction]
    genFraction (Precision p)
      | p >= 16   = error $ "stats: precision " ++ show p ++ " too high"
      | otherwise = [
            ProperFraction $ fromIntegral x / fromIntegral ((2 :: Word) ^ p)
          | x <- [0 .. (2 :: Word) ^ p - 1]
          ]

    count :: Map a Word -> [a] -> [(a, Percentage)]
    count acc (x:xs) = count (Map.alter (Just . (+1) . fromMaybe 0) x acc) xs
    count acc []     = map (second asPct) $ Map.toList acc
      where
        total :: Word
        total = sum $ Map.elems acc

        asPct :: Word -> Percentage
        asPct c = Percentage (fromIntegral c / fromIntegral total) (c == 0)


