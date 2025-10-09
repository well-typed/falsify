module Test.Falsify.Internal.Property where

import Data.Map (Map)
import Data.Set (Set)
import GHC.Exception (CallStack)

data TestResult e a =
    TestPassed a
  | TestFailed e
  | TestDiscarded

data TestRun = TestRun {
      runLog :: Log
    , runDeterministic :: Bool
    , runLabels :: Map String (Set String)
    }

data LogEntry =
    -- | Generated a value
    --
    -- We record the value that was generated as well as /where/ we generated it
    Generated CallStack String

    -- | Some additional information
  | Info String

newtype Log = Log [LogEntry]
