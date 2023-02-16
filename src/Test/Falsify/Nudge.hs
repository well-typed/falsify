-- | Nudge values
--
-- Intended for unqualified import.
module Test.Falsify.Nudge (
    NudgeBy(..)
    -- * Offsets
  , NoOffset(..)
  , Offset(..)
  ) where

{-------------------------------------------------------------------------------
  Offsets
-------------------------------------------------------------------------------}

data NoOffset = NoOffset
  deriving stock (Show, Eq)

newtype Offset a = Offset a
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

class NudgeBy o a where
  nudgeUp   :: o -> a -> a
  nudgeDown :: o -> a -> a

instance NudgeBy NoOffset a where
  nudgeUp   NoOffset = id
  nudgeDown NoOffset = id

instance Num a => NudgeBy (Offset a) a where
  nudgeUp   (Offset o) x = x + o
  nudgeDown (Offset o) x = x - o

