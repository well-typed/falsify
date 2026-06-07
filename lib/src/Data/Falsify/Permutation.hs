-- | Permutations
--
-- Intended for qualified import.
--
-- > import Data.Falsify.Permutation (Permutation)
-- > import qualified Data.Falsify.Permutation as Permutation
module Data.Falsify.Permutation (
    Permutation -- opaque
  , invariant
  , toSwaps
    -- * Construction
  , identity
  , fromSwaps
    -- * Properties
  , size
    -- * Using permutations
  , apply
  ) where

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Permutation is a sequence of swaps
newtype Permutation = Permutation {
      -- | Individual swaps executed by this permutation
      toSwaps :: [(Word, Word)]
    }
  deriving stock (Show)

-- | Permutation invariant
--
-- For every swap @(i, j)@ in the permutation we must have @i > j@.
invariant :: Permutation -> Bool
invariant = all checkSwap . toSwaps
  where
    checkSwap :: (Word, Word) -> Bool
    checkSwap (i, j) = i > j

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Identity permutation
identity :: Permutation
identity = fromSwaps []

-- | From swaps
--
-- Any identity swaps are filtered out.
fromSwaps :: [(Word, Word)] -> Permutation
fromSwaps = Permutation . filter (\(i, j) -> i /= j)

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Number of swaps
size :: Permutation -> Int
size = length . toSwaps

{-------------------------------------------------------------------------------
  Using permutations
-------------------------------------------------------------------------------}

-- | Apply permutation
apply :: Permutation -> [a] -> [a]
apply (Permutation p) xs =
    V.toList $ V.modify (forM_ (map conv p) . swap) (V.fromList xs)
  where
    swap :: V.MVector s a -> (Int, Int) -> ST s ()
    swap vec (i, j) = do
        x <- VM.read vec i
        y <- VM.read vec j
        VM.write vec i y
        VM.write vec j x

    conv :: (Word, Word) -> (Int, Int)
    conv (i, j) = (fromIntegral i, fromIntegral j)
