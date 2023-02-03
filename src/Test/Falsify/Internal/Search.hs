module Test.Falsify.Internal.Search (
    -- * Binary search
    binarySearch
  , binarySearchNoParityBias
  ) where

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
-- value of opposite parity; this means that we avoid any bias towards even or
-- odd numbers.
--
-- Examples:
--
-- > binarySearchNoParityBias   0 == []
-- > binarySearchNoParityBias   1 == [0]
-- > binarySearchNoParityBias   2 == [0,1]
-- > binarySearchNoParityBias   3 == [0,1,2]
-- > binarySearchNoParityBias   4 == [0,1,2,3]
-- > binarySearchNoParityBias   5 == [0,1,2,3,4]
-- > binarySearchNoParityBias   6 == [0,1,2,3,4,5]
-- > binarySearchNoParityBias   7 == [0,1,4,5,6]
-- > binarySearchNoParityBias   8 == [0,1,4,5,6,7]
-- > binarySearchNoParityBias   9 == [0,1,4,5,6,7,8]
-- > binarySearchNoParityBias  10 == [0,1,4,5,8,9]
-- > binarySearchNoParityBias  16 == [0,1,8,9,12,13,14,15]
-- > binarySearchNoParityBias 127 == [0,1,64,65,96,97,112,113,120,121,124,125,126]
-- > binarySearchNoParityBias 128 == [0,1,64,65,96,97,112,113,120,121,124,125,126,127]
binarySearchNoParityBias :: Word64 -> [Word64]
binarySearchNoParityBias = adjust . binarySearch
  where
    -- Starting with some value @y@, 'binarySearch' has given us a list of
    -- possible shrunk values
    --
    -- > x0, x1 .. xN
    --
    -- We will pair each @x@ with a value @x'@ by flipping the LSB of @x@;
    -- @x@ and @x'@ will then be considered to be the same value by the
    -- 'signedWord63' generator, the primarily user of this shrinker.
    --
    -- We have to be careful not to exceed the boundaries and not to introduce
    -- any duplicates.
    --
    -- (a) At the lower end of the range, if @x0@ exists (i.e., if we can shrink
    --     at all), we must have @x0 == 0@; flipping the LSB would result in
    --     @1@, which we can introduce unless
    --
    --     (i)  @x0@ is the /only/ value, in which case it is also the upper
    --          limit. Since the final value must be exactly one less than the
    --          value @y@ we started with, @y@ must have been 1, which means
    --          we just executed a bitflip, and there is no need to introduce
    --          any additional values.
    --     (ii) @x1 == 1@ (we should avoid introducing duplicates)
    --
    -- (b) At the upper end of the range:
    --
    --     (i)  If @xN@ is even, then flipping the LSB would result in @xN + 1@,
    --          outside of the range. However, since @xN@ must be precisely one
    --          less than the value @N@ we are trying to shrink, we effectively
    --          just /did/ a bit-flip, just like in (a.i), above.
    --     (ii) If @xN@ is odd, flipping the LSB would result in @xN - 1@,
    --          which we can introduce unless that is identical to the previous
    --          value.
    --
    -- (c) Otherwise
    --
    --     (i)  If @xi@ is even, we must introduce @xi + 1@ unless that is
    --          equal to the next value.
    --     (ii) If @xi@ is odd, we must introduce @xi - 1@ unless that is
    --          equal to the previous value.
    adjust :: [Word64] -> [Word64]
    adjust []       = []
    adjust [0]      = [0]                   -- (a.i)
    adjust (0:1:xs) = 0 :     go 0 (1:xs)   -- (a.ii)
    adjust (0:x:xs) = 0 : 1 : go 1 (x:xs)
    adjust _        = error "if we can shrink at all, first value must be 0"

    go :: Word64 -> [Word64] -> [Word64]
    go _    []    = []
    go prev [xN]
      | even xN   = [xN]                    -- (b.i)
      | otherwise = if prev == pred xN      -- (b.ii)
                      then [xN]
                      else [pred xN, xN]
    go prev (x:next:xs)
      | even x    = if next == succ x       -- (c.i)
                      then x : go x (next:xs)
                      else x : succ x : go (succ x) (next:xs)
      | otherwise = if prev == pred x       -- (c.ii)
                      then x : go x (next:xs)
                      else pred x : x : go x (next:xs)

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
