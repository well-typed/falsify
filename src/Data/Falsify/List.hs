module Data.Falsify.List (
    -- * Predicates
    pairwiseAll
  , pairwiseAny
    -- * Permutations
  , Permutation
  , applyPermutation
    -- * Dealing with marks
  , genKept
  , keepAtLeast
  ) where

import Control.Monad
import Control.Monad.ST
import Control.Selective (Selective)

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import Data.Falsify.Marked

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

{-------------------------------------------------------------------------------
  Dealing with marks
-------------------------------------------------------------------------------}

genKept :: forall f a. Selective f => [Marked (f a)] -> f [a]
genKept = go
  where
    go :: [Marked (f a)] -> f [a]
    go []     = pure []
    go (m:ms) = cons <$> selectKept m <*> go  ms

    cons :: Maybe a -> [a] -> [a]
    cons Nothing  = id
    cons (Just x) = (x:)

keepAtLeast :: Word -> [Marked a] -> [Marked a]
keepAtLeast = \n xs ->
    let kept = countKept xs
    in if kept >= n
         then xs
         else go (n - kept) xs
  where
    go :: Word -> [Marked a] -> [Marked a]
    go _ []                 = []
    go 0 xs                 = xs
    go n (Marked Keep x:xs) = Marked Keep x : go  n      xs
    go n (Marked Drop x:xs) = Marked Keep x : go (n - 1) xs
