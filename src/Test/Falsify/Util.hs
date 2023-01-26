-- | Miscellaneous utility functions
--
-- Intended for unqualified import.
module Test.Falsify.Util (
    -- * Endian conversions
    fromLittleEndian
  , toLittleEndian
  ) where

import Data.Bits
import Data.Word

{-------------------------------------------------------------------------------
  Endian conversions
-------------------------------------------------------------------------------}

-- | Little endian interpretation
--
-- Examples:
--
-- > fromLittleEndian []           == 0
-- > fromLittleEndian [0x01]       == 1
-- > fromLittleEndian [0x01, 0x02] == 513
-- > fromLittleEndian [0x02, 0x02] == 514
--
-- The reason we choose little endian as our basis rather than big endian is
-- that the result is then invariant under truncation of zeroes:
--
-- >    fromLittleEndian [0x01, 0x00, 0x00]
-- > == fromLittleEndian [0x01]
-- > == 1
fromLittleEndian :: [Word8] -> Word64
fromLittleEndian = go 0 . reverse
  where
    go :: Word64 -> [Word8] -> Word64
    go acc []     = acc
    go acc (x:xs) = go (shift acc 8 .|. fromIntegral x) xs

-- | Inverse of 'fromLittleEndian'
toLittleEndian :: Word64 -> [Word8]
toLittleEndian = go
  where
    go :: Word64 -> [Word8]
    go 0 = []
    go x = fromIntegral (x .&. 0xFF) : go (shift x (-8))

