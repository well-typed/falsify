module Data.Falsify.List (
    -- * Predicates
    pairwiseAll
  , pairwiseAny
    -- * Permutations
  , Permutation
  , applyPermutation
  ) where

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

{-------------------------------------------------------------------------------
  Predicates
-------------------------------------------------------------------------------}

pairwiseAll :: forall a. (a -> a -> Bool) -> [a] -> Bool
pairwiseAll p = go
  where
    go :: [a] -> Bool
    go []       = True
    go [_]      = True
    go (x:y:zs) = p x y && go (y:zs)

pairwiseAny :: forall a. (a -> a -> Bool) -> [a] -> Bool
pairwiseAny p = go
  where
    go :: [a] -> Bool
    go []       = False
    go [_]      = False
    go (x:y:zs) = p x y || go (y:zs)

{-------------------------------------------------------------------------------
  Permutations
-------------------------------------------------------------------------------}

-- | Permutation is a sequence of swaps
type Permutation = [(Word, Word)]

applyPermutation :: Permutation -> [a] -> [a]
applyPermutation p xs =
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
