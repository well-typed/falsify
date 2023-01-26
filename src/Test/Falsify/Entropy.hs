-- | Entropy
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Entropy (Entropy)
-- > import qualified Test.Falsify.Entropy as Entropy
module Test.Falsify.Entropy (
    Entropy(..)
  ) where

import Data.Word
import Data.List

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype Entropy = Entropy {
      getEntropy :: [Word8]
    }
  deriving newtype (Semigroup, Monoid)

instance Show Entropy where
  show (Entropy xs) =
      case splitAt 10 xs of
        (prefix, []) -> "[" ++ intercalate "," (map show prefix) ++ "]"
        (prefix, _)  -> "[" ++ intercalate "," (map show prefix) ++ "..]"

