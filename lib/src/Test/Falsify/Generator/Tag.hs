module Test.Falsify.Generator.Tag
  ( Falsify
  ) where

import Test.Falsify.Generator.Default (ViaIntegral (..), GenDefault, ViaEnum (..), ViaGeneric (..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

-- | Type tag for these built-in default generators.
-- You can use this tag directly or choose type-by-type with 'ViaTag'.
data Falsify

deriving via (ViaEnum ()) instance GenDefault Falsify ()
deriving via (ViaEnum Bool) instance GenDefault Falsify Bool
deriving via (ViaEnum Char) instance GenDefault Falsify Char

deriving via (ViaIntegral Int) instance GenDefault Falsify Int
deriving via (ViaIntegral Int8) instance GenDefault Falsify Int8
deriving via (ViaIntegral Int16) instance GenDefault Falsify Int16
deriving via (ViaIntegral Int32) instance GenDefault Falsify Int32
deriving via (ViaIntegral Int64) instance GenDefault Falsify Int64

deriving via (ViaIntegral Word) instance GenDefault Falsify Word
deriving via (ViaIntegral Word8) instance GenDefault Falsify Word8
deriving via (ViaIntegral Word16) instance GenDefault Falsify Word16
deriving via (ViaIntegral Word32) instance GenDefault Falsify Word32
deriving via (ViaIntegral Word64) instance GenDefault Falsify Word64

deriving via (ViaGeneric Falsify (Maybe a))
  instance GenDefault Falsify a => GenDefault Falsify (Maybe a)
deriving via (ViaGeneric Falsify (Either a b))
  instance (GenDefault Falsify a, GenDefault Falsify b) => GenDefault Falsify (Either a b)

deriving via
  (ViaGeneric Falsify (a, b))
  instance
    (GenDefault Falsify a, GenDefault Falsify b)
    => GenDefault Falsify (a, b)

deriving via
  (ViaGeneric Falsify (a, b, c))
  instance
    (GenDefault Falsify a, GenDefault Falsify b, GenDefault Falsify c)
    => GenDefault Falsify (a, b, c)

deriving via
  (ViaGeneric Falsify (a, b, c, d))
  instance
    (GenDefault Falsify a, GenDefault Falsify b, GenDefault Falsify c, GenDefault Falsify d)
    => GenDefault Falsify (a, b, c, d)

deriving via
  (ViaGeneric Falsify (a, b, c, d, e))
  instance
    (GenDefault Falsify a, GenDefault Falsify b, GenDefault Falsify c, GenDefault Falsify d, GenDefault Falsify e)
    => GenDefault Falsify (a, b, c, d, e)
