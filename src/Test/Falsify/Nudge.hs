-- | Nudge values
--
-- Intended for unqualified import.
module Test.Falsify.Nudge (
    NudgeBy(..)
  , NoOffset(..)
  ) where

{-------------------------------------------------------------------------------
  Offsets
-------------------------------------------------------------------------------}

data NoOffset = NoOffset
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

class NudgeBy o a where
  nudgeUp   :: o -> a -> a
  nudgeDown :: o -> a -> a

instance NudgeBy NoOffset a where
  nudgeUp   NoOffset = id
  nudgeDown NoOffset = id

instance Num a => NudgeBy Word a where
  nudgeUp   o x = x + fromIntegral o
  nudgeDown o x = x - fromIntegral o

