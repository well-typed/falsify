module Data.Falsify.Integer (
    -- * Encoding
    Bit(..)
    -- ** Binary
  , natToBits
  , natFromBits
    -- ** Elias γ code
  , encEliasG
  , decEliasG
  , encIntegerEliasG
  , decIntegerEliasG
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Bits
import Numeric.Natural

{-------------------------------------------------------------------------------
  Binary encoding
-------------------------------------------------------------------------------}

data Bit = I | O
  deriving (Show, Eq, Ord)

-- | Binary encoding (most significant bit first)
natToBits :: Natural -> [Bit]
natToBits = \n -> if
  | n < 0     -> error "toBits: negative input"
  | n == 0    -> []
  | otherwise -> reverse $ go n
  where
    go :: Natural -> [Bit]
    go 0 = []
    go n = (if testBit n 0 then I else O) : go (shiftR n 1)

-- | Inverse to 'toBits'
natFromBits :: [Bit] -> Natural
natFromBits = go . reverse
  where
    go :: [Bit] -> Natural
    go []       = 0
    go (O : bs) = shiftL (go bs) 1
    go (I : bs) = shiftL (go bs) 1 + 1

{-------------------------------------------------------------------------------
  Elias γ code
-------------------------------------------------------------------------------}

-- | Elias γ code
--
-- Precondition: input @x >= 1@.
--
-- See <https://en.wikipedia.org/wiki/Elias_gamma_coding> .
encEliasG :: Natural -> [Bit]
encEliasG x
  | x == 0    = error "eliasG: zero"
  | otherwise = zeroes x
  where
    zeroes :: Natural -> [Bit]
    zeroes n
      | n <= 1    = natToBits x
      | otherwise = O : zeroes (shiftR n 1)

-- | Inverse of 'encEliasG'
--
-- Returns the decd number and the remaining part of the bitstream.
decEliasG :: [Bit] -> Maybe (Natural, [Bit])
decEliasG bs = do
    let (zeroes, afterZeroes) = span (== O) bs
        (number, afterNumber) = splitAt (length zeroes + 1) afterZeroes
    guard $ length number == length zeroes + 1
    return (natFromBits number, afterNumber)

-- | Extension of Elias γ coding to signed integers
--
-- This is adapted from @integerVariant@ in @Test.QuickCheck.Random@. The first
-- bit encs whether @x >= 1@ or not (this will result in @0@ and @1@ having
-- short codes).
encIntegerEliasG :: Integer -> [Bit]
encIntegerEliasG = \x ->
    if x >= 1
      then O : encEliasG (fromInteger          $ x)
      else I : encEliasG (fromInteger . mangle $ x)
  where
    mangle :: Integer -> Integer
    mangle x = 1 - x

-- | Inverse to 'encIntegerEliasG'
decIntegerEliasG :: [Bit] -> Maybe (Integer, [Bit])
decIntegerEliasG []       = Nothing
decIntegerEliasG (O : bs) = first             toInteger  <$> decEliasG bs
decIntegerEliasG (I : bs) = first (unmangle . toInteger) <$> decEliasG bs
  where
    unmangle :: Integer -> Integer
    unmangle y = 1 - y

