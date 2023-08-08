module Test.Falsify.GenDefault.Std
  ( Std
  ) where

import Test.Falsify.GenDefault (ViaIntegral (..), GenDefault, ViaEnum (..), ViaGeneric (..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

-- | Type tag for these "standard" default generators.
-- You can use this tag directly or choose type-by-type with 'ViaTag'.
data Std

deriving via (ViaEnum ()) instance GenDefault Std ()
deriving via (ViaEnum Bool) instance GenDefault Std Bool
deriving via (ViaEnum Char) instance GenDefault Std Char

deriving via (ViaIntegral Int) instance GenDefault Std Int
deriving via (ViaIntegral Int8) instance GenDefault Std Int8
deriving via (ViaIntegral Int16) instance GenDefault Std Int16
deriving via (ViaIntegral Int32) instance GenDefault Std Int32
deriving via (ViaIntegral Int64) instance GenDefault Std Int64

deriving via (ViaIntegral Word) instance GenDefault Std Word
deriving via (ViaIntegral Word8) instance GenDefault Std Word8
deriving via (ViaIntegral Word16) instance GenDefault Std Word16
deriving via (ViaIntegral Word32) instance GenDefault Std Word32
deriving via (ViaIntegral Word64) instance GenDefault Std Word64

deriving via (ViaGeneric Std (Maybe a))
  instance GenDefault Std a => GenDefault Std (Maybe a)

deriving via (ViaGeneric Std (Either a b))
  instance (GenDefault Std a, GenDefault Std b) => GenDefault Std (Either a b)

deriving via
  (ViaGeneric Std (a, b))
  instance
    (GenDefault Std a, GenDefault Std b)
    => GenDefault Std (a, b)

deriving via
  (ViaGeneric Std (a, b, c))
  instance
    (GenDefault Std a, GenDefault Std b, GenDefault Std c)
    => GenDefault Std (a, b, c)

deriving via
  (ViaGeneric Std (a, b, c, d))
  instance
    (GenDefault Std a, GenDefault Std b, GenDefault Std c, GenDefault Std d)
    => GenDefault Std (a, b, c, d)

deriving via
  (ViaGeneric Std (a, b, c, d, e))
  instance
    (GenDefault Std a, GenDefault Std b, GenDefault Std c, GenDefault Std d, GenDefault Std e)
    => GenDefault Std (a, b, c, d, e)
