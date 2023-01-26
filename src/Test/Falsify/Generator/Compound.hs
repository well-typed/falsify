module Test.Falsify.Generator.Compound (
    listOfMaxLength
  ) where

import Control.Monad
import Data.Maybe
import Data.Word

import Test.Falsify.Generator (GenT)

import qualified Test.Falsify.Generator as Gen

listOfMaxLength :: Monad m => Word -> GenT m a -> GenT m [a]
listOfMaxLength n g =
    catMaybes <$> replicateM (fromIntegral n) (aux <$> Gen.byte <*> g)
  where
    aux :: Word8 -> a -> Maybe a
    aux 0 = const Nothing
    aux _ = Just
