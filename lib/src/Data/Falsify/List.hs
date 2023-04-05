module Data.Falsify.List (
    -- * Splitting
    chunksOfNonEmpty
    -- * Permutations
  , Permutation
  , applyPermutation
    -- * Dealing with marks
  , keepAtLeast
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import Data.Falsify.Marked

{-------------------------------------------------------------------------------
  Splitting
-------------------------------------------------------------------------------}

-- | Take chunks of a non-empty list
--
-- This is lazy:
--
-- >    NE.take 4 $ chunksOfNonEmpty 3 (0 :| [1..])
-- > == [ 0 :| [1,2]
-- >    , 3 :| [4,5]
-- >    , 6 :| [7,8]
-- >    , 9 :| [10,11]
-- >    ]
chunksOfNonEmpty :: Word -> NonEmpty a -> NonEmpty (NonEmpty a)
chunksOfNonEmpty 0 _         = error "chunksOfNonEmpty: zero chunk size"
chunksOfNonEmpty n (x :| xs) =
    let (chunk, rest) = splitAt (fromIntegral n) (x : xs)
    in case (chunk, rest) of
         ([]   , _)    -> error "impossible"
         (c:cs , [])   -> (c :| cs) :| []
         (c:cs , r:rs) -> (c :| cs) :| toList (chunksOfNonEmpty n (r :| rs))

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

keepAtLeast :: Word -> [Marked f a] -> [Marked f a]
keepAtLeast = \n xs ->
    let kept = countKept xs
    in if kept >= n
         then xs
         else go (n - kept) xs
  where
    go :: Word -> [Marked f a] -> [Marked f a]
    go _ []                 = []
    go 0 xs                 = xs
    go n (Marked Keep x:xs) = Marked Keep x : go  n      xs
    go n (Marked Drop x:xs) = Marked Keep x : go (n - 1) xs
