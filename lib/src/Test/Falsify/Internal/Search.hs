module Test.Falsify.Internal.Search (
    -- * Binary search
    binarySearch
  , binarySearchNoParityBias
  ) where

import Data.Bits
import Data.List (nub)
import Data.Word

{-------------------------------------------------------------------------------
  Binary search
-------------------------------------------------------------------------------}

-- | Binary search
--
-- Compute one step of a binary search algorithm.
--
-- Examples:
--
-- > binarySearch   0 == []
-- > binarySearch   1 == [0]
-- > binarySearch   2 == [0,1]
-- > binarySearch   3 == [0,2]
-- > binarySearch   4 == [0,2,3]
-- > binarySearch   5 == [0,3,4]
-- > binarySearch   6 == [0,3,5]
-- > binarySearch   7 == [0,4,6]
-- > binarySearch   8 == [0,4,6,7]
-- > binarySearch   9 == [0,5,7,8]
-- > binarySearch  10 == [0,5,8,9]
-- > binarySearch  16 == [0,8,12,14,15]
-- > binarySearch 127 == [0,64,96,112,120,124,126]
-- > binarySearch 128 == [0,64,96,112,120,124,126,127]
--
-- The gap between each successive number halves at each step.
--
-- NOTE: 'binarySearch' introduces a bias for even numbers: when shrinking
-- succeeds with the first (non-zero) option, the number is basically halved
-- each at step; since halving an even number results in another even number,
-- and halving an odd number /also/ results in an even number, this results in a
-- strong bias towards even numbers. See also 'binarySearchNoParityBias'.
binarySearch :: Word64 -> [Word64]
binarySearch = go 0 . deltas
  where
    go :: Word64 -> [Word64] -> [Word64]
    go _ []     = []
    go n (d:ds) = n : go (n + d) ds

-- | Binary search without parity bias
--
-- For some cases the parity (even or odd) of a number is very important, and
-- unfotunately standard binary search is not very good at allowing search to
-- flip between even and odd. For example, if we start with 'maxBound',
-- /every/ possibly shrink value computed by 'binarySearch' is even. The
-- situation is less extreme for other numbers, but it's nonetheless something
-- we need to take into account.
--
-- In this function we pair each possible shrunk value with the corresponding
-- value of opposite parity, ordered in such a way that we try to shrink to
-- opposite parity first.
--
-- Examples:
--
-- > binarySearchNoParityBias   0 == []
-- > binarySearchNoParityBias   1 == [0]
-- > binarySearchNoParityBias   2 == [1,0]
-- > binarySearchNoParityBias   3 == [0,1,2]
-- > binarySearchNoParityBias   4 == [1,0,3,2]
-- > binarySearchNoParityBias   5 == [0,1,2,3,4]
-- > binarySearchNoParityBias   6 == [1,0,3,2,5,4]
-- > binarySearchNoParityBias   7 == [0,1,4,5,6]
-- > binarySearchNoParityBias   8 == [1,0,5,4,7,6]
-- > binarySearchNoParityBias   9 == [0,1,4,5,6,7,8]
-- > binarySearchNoParityBias  10 == [1,0,5,4,9,8]
-- > binarySearchNoParityBias  16 == [1,0,9,8,13,12,15,14]
-- > binarySearchNoParityBias 127 == [0,1,64,65,96,97,112,113,120,121,124,125,126]
-- > binarySearchNoParityBias 128 == [1,0,65,64,97,96,113,112,121,120,125,124,127,126]
binarySearchNoParityBias :: Word64 -> [Word64]
binarySearchNoParityBias y =
    filter (< y) . nub . concatMap pairWithOpposite $
      binarySearch y
  where
    pairWithOpposite :: Word64 -> [Word64]
    pairWithOpposite x
      | even x == even y = [x `xor` 1, x]
      | otherwise        = [x, x `xor` 1]

-- | Auxiliary to 'binarySearch'
--
-- Given a number @n@, compute a set of steps @n1, n2, ..@ such that
-- @sum [n1, n2, ..] == n@, the distance between each subsequent step
-- is halved, and all steps are non-zero. For example:
--
-- > deltas 200 == [100,50,25,12,6,3,2,1,1]
deltas :: Word64 -> [Word64]
deltas 0 = []
deltas 1 = [1]
deltas n
  | even n    = mid     : deltas mid
  | otherwise = mid + 1 : deltas mid
  where
    mid = n `div` 2
