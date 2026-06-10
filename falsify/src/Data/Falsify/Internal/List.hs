module Data.Falsify.Internal.List (
    -- * Splitting
    chunksOfNonEmpty
    -- * Dealing with marks
  , keepAtLeast
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))

import Test.Falsify.Marked (Mark(..), Marked(..))
import qualified Test.Falsify.Marked as Marked

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
  Dealing with marks
-------------------------------------------------------------------------------}

keepAtLeast :: Word -> [Marked f a] -> [Marked f a]
keepAtLeast = \n xs ->
    let kept = Marked.countKept xs
    in if kept >= n
         then xs
         else go (n - kept) xs
  where
    go :: Word -> [Marked f a] -> [Marked f a]
    go _ []                 = []
    go 0 xs                 = xs
    go n (Marked Keep x:xs) = Marked Keep x : go  n      xs
    go n (Marked Drop x:xs) = Marked Keep x : go (n - 1) xs
