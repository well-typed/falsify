{-# LANGUAGE CPP #-}

-- | Replay seeds
--
-- We need a seed/gamma pair to initialize a splitmix PRNG. This is however a
-- pretty low level implementation detail that I'd prefer not be be directly
-- visible. We therefore provide a thin layer on top, which provides an
-- "encoded" replay seed. This has the additional benefits that the length of
-- the replay seed is always the same (unlike just writing a 'Word64'), and we
-- could in principle at some point support other kinds of PRNGs.
module Test.Falsify.Internal.Driver.ReplaySeed (
    ReplaySeed(..)
  , parseReplaySeed
  , safeReadReplaySeed
  , splitmixReplaySeed
  ) where

import Data.String
import Data.Word
import Data.Binary
import System.Random.SplitMix

import qualified Data.ByteString.Base16.Lazy as Lazy.Base16
import qualified Data.ByteString.Lazy.Char8  as Lazy.Char8

data ReplaySeed =
    ReplaySplitmix Word64 Word64

splitmixReplaySeed :: SMGen -> ReplaySeed
splitmixReplaySeed = uncurry ReplaySplitmix . unseedSMGen

instance Binary ReplaySeed where
  put (ReplaySplitmix seed gamma) = do
      putWord8 1
      put seed
      put gamma

  get = do
      tag <- getWord8
      case tag of
        1 -> do seed  <- get
                gamma <- get
                if odd gamma
                  then return $ ReplaySplitmix seed gamma
                  else fail $ "ReplaySeed: expected odd gamma for splitmix"
        n -> fail $ "ReplaySeed: invalid tag: " ++ show n

instance Show ReplaySeed where
  show = Lazy.Char8.unpack . Lazy.Base16.encode . encode

instance IsString ReplaySeed where
  fromString = aux . safeReadReplaySeed
    where
      aux :: Maybe ReplaySeed -> ReplaySeed
      aux Nothing  = error "ReplaySeed: invalid seed"
      aux (Just s) = s

safeReadReplaySeed :: String -> Maybe ReplaySeed
safeReadReplaySeed = parseReplaySeed

#if MIN_VERSION_base(4,13,0)
parseReplaySeed :: forall m. MonadFail m => String -> m ReplaySeed
#else
parseReplaySeed :: forall m. Monad m => String -> m ReplaySeed
#endif

parseReplaySeed str = do
    raw <- case Lazy.Base16.decode (Lazy.Char8.pack str) of
             Left err -> fail err
             Right x  -> return x
    case decodeOrFail raw of
      Left  (_, _, err) -> fail err
      Right (_, _, x)   -> return x
