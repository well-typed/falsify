-- | Nudge values
--
-- Intended for unqualified import.
module Test.Falsify.Nudge (
    Nudge(..)
  , NoOffset(..)
  ) where

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

class Nudge a o where
  nudgeUp   :: o -> a -> a
  nudgeDown :: o -> a -> a

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

data NoOffset = NoOffset

instance Nudge a NoOffset where
  nudgeUp   NoOffset = id
  nudgeDown NoOffset = id

instance Num a => Nudge a Word where
  nudgeUp   o x = x + fromIntegral o
  nudgeDown o x = x - fromIntegral o

