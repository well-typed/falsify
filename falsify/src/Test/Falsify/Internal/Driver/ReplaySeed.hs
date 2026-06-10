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
  , splitmixReplaySeed
  ) where

import Data.String
import Data.Word
import Data.Binary
import System.Random.SplitMix

import qualified Data.ByteString.Base16.Lazy as Lazy.Base16
import qualified Data.ByteString.Lazy.Char8  as Lazy.Char8

-- | Replay seed
--
-- By default, when we falsify a property we start with a PRNG initialized using
-- a random seed (produced using the system entropy; this relies on
-- 'System.Random.SplitMix.initSMGen' in @splitmix@). When a property /fails/,
-- we will report the exact seed used, so that the user can re-run the exact
-- same test again, if desired.
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
  fromString = aux . parseReplaySeed
    where
      aux :: Either String ReplaySeed -> ReplaySeed
      aux (Left  err)  = error $ "ReplaySeed: invalid seed: " ++ err
      aux (Right seed) = seed

-- | Parse 'ReplaySeed'
--
-- Returns 'Left' an error message if parsing failed.
parseReplaySeed :: String -> Either String ReplaySeed
parseReplaySeed str = do
    raw <- case Lazy.Base16.decode (Lazy.Char8.pack str) of
             Left err -> Left err
             Right x  -> Right x
    case decodeOrFail raw of
      Left  (_, _, err) -> Left err
      Right (_, _, x)   -> return x
