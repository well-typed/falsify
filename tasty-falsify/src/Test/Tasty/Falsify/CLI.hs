{-# OPTIONS_GHC -Wno-orphans #-}

-- | Command line flags
module Test.Tasty.Falsify.CLI (
    Tests(..)
  , MaxShrinks(..)
  , Replay(..)
  , MaxRatio(..)
  , extractFalsifyCliFlags
  , allFalsifyCliFlags
  ) where

import Data.Default
import Data.Tagged
import Test.Tasty.Options (IsOption(..))
import Data.Proxy

import qualified Options.Applicative as Opts
import qualified Test.Tasty.Options  as Tasty

import Test.Falsify.Driver (ReplaySeed, parseReplaySeed)
import qualified Test.Falsify.Driver as Falsify

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype Tests      = Tests      { getTests      :: Word             }
newtype MaxShrinks = MaxShrinks { getMaxShrinks :: Maybe Word       }
newtype Replay     = Replay     { getReplay     :: Maybe ReplaySeed }
newtype MaxRatio   = MaxRatio   { getMaxRatio   :: Word             }

-- | Extract CLI options
extractFalsifyCliFlags :: Tasty.OptionSet -> (Falsify.Verbose, Falsify.Options)
extractFalsifyCliFlags optionSet = (
      Tasty.lookupOption optionSet
    , Falsify.Options {
          tests      = getTests      $ Tasty.lookupOption optionSet
        , maxShrinks = getMaxShrinks $ Tasty.lookupOption optionSet
        , replay     = getReplay     $ Tasty.lookupOption optionSet
        , maxRatio   = getMaxRatio   $ Tasty.lookupOption optionSet
        }
    )

-- | Make @tasty@ aware of the @falsify@ specific CLI flags
allFalsifyCliFlags :: [Tasty.OptionDescription]
allFalsifyCliFlags = [
      Tasty.Option $ Proxy @Falsify.Verbose
    , Tasty.Option $ Proxy @Tests
    , Tasty.Option $ Proxy @MaxShrinks
    , Tasty.Option $ Proxy @Replay
    , Tasty.Option $ Proxy @MaxRatio
    ]

{-------------------------------------------------------------------------------
  'IsOption' instances
-------------------------------------------------------------------------------}

instance IsOption Tests where
  defaultValue   = Tests (Falsify.tests def)
  parseValue     = fmap Tests . Tasty.safeRead . filter (/= '_')
  optionName     = Tagged "falsify-tests"
  optionHelp     = Tagged "Number of test cases to generate"

instance IsOption MaxShrinks where
  defaultValue   = MaxShrinks (Falsify.maxShrinks def)
  parseValue     = fmap (MaxShrinks . Just) . Tasty.safeRead
  optionName     = Tagged "falsify-shrinks"
  optionHelp     = Tagged "Random seed to use for replaying a previous test run"

instance IsOption Replay where
  defaultValue   = Replay (Falsify.replay def)
  parseValue     = aux . parseReplaySeed
    where
      -- Slightly confusing here: the /outer/ 'Maybe' corresponds to test
      -- failure, the /inner/ 'Maybe' to the fact that the seed is optional.
      aux :: Either String ReplaySeed -> Maybe Replay
      aux (Left  _err) = Nothing
      aux (Right seed) = Just $ Replay (Just seed)
  optionName     = Tagged "falsify-replay"
  optionHelp     = Tagged "Random seed to use for replaying test"
  optionCLParser = Opts.option (Opts.str >>= aux . parseReplaySeed) $ mconcat [
                       Opts.long $ untag $ optionName @Replay
                     , Opts.help $ untag $ optionHelp @Replay
                     ]
    where
      aux :: Either String ReplaySeed -> Opts.ReadM Replay
      aux (Left  err)  = fail err
      aux (Right seed) = return $ Replay (Just seed)

instance IsOption MaxRatio where
  defaultValue   = MaxRatio (Falsify.maxRatio def)
  parseValue     = fmap MaxRatio . Tasty.safeRead . filter (/= '_')
  optionName     = Tagged "falsify-max-ratio"
  optionHelp     = Tagged "Maximum number of discarded tests per successful test"

instance IsOption Falsify.Verbose where
  defaultValue   = Falsify.NotVerbose
  parseValue     = fmap ( \b -> if b then Falsify.Verbose
                                     else Falsify.NotVerbose
                        )
                 . Tasty.safeReadBool
  optionName     = Tagged $ "falsify-verbose"
  optionHelp     = Tagged $ "Show the generated test cases"
  optionCLParser = Tasty.mkFlagCLParser mempty Falsify.Verbose
