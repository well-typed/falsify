-- | Properties
--
-- Intended for unqualified import.
module Test.Falsify.Internal.Property (
    -- * Property
    Property -- opaque
  , runProperty
    -- * State
  , TestRun(..)
  , Log(..)
  , LogEntry(..)
    -- * Construction
  , gen
  , genWith
  , info
  , assert
  ) where

import Prelude hiding (log)

import Control.Monad.Except
import Control.Monad.State
import GHC.Stack

import Test.Falsify.Generator (Gen)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Property
--
-- A 'Property' is a generator that can fail and keeps a track of some
-- information about the test run.
newtype Property a = Property {
    unwrapProperty :: ExceptT String (StateT TestRun Gen) a
  }
  deriving newtype (Functor, Applicative, Monad)

-- | Construct property
--
-- This is a low-level function for internal use only.
mkProperty ::
     (TestRun -> Gen (Either String a, TestRun))
  -> Property a
mkProperty = Property . ExceptT . StateT

-- | Run property
runProperty :: Property a -> Gen (Either String a, TestRun)
runProperty = flip runStateT initTestRun . runExceptT . unwrapProperty

{-------------------------------------------------------------------------------
  Property state
-------------------------------------------------------------------------------}

data TestRun = TestRun {
      runLog :: Log

      -- | Did we generate any values in this test run?
      --
      -- If not, there is no point running the test more than once (with
      -- different seeds), since the test is deterministic.
    , runDeterministic :: Bool
    }

data LogEntry =
    -- | Generated a value
    --
    -- We record the value that was generated as well as /where/ we generated it
    Generated CallStack String

    -- | Some additional information
  | Info CallStack String
  deriving (Show)

-- | Log of the events happened during a test run
--
-- The events are recorded in reverse chronological order
newtype Log = Log [LogEntry]
  deriving (Show)

initTestRun :: TestRun
initTestRun = TestRun {
      runLog           = Log []
    , runDeterministic = True
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Generate value and add it to the log
gen :: (HasCallStack, Show a) => Gen a -> Property a
gen = genWith show

-- | Generalization of 'gen' that doesn't depend on a 'Show' instance
genWith :: forall a. HasCallStack => (a -> String) -> Gen a -> Property a
genWith f g = mkProperty $ \run -> aux run <$> g
  where
    aux :: TestRun -> a -> (Either String a, TestRun)
    aux run@TestRun{runLog = Log log} x = (
          Right x
        , run{ runLog           = Log $ Generated callStack (f x) : log
             , runDeterministic = False
             }
        )

-- | Log some additional information about the test
--
-- This will be shown in verbose mode.
info :: HasCallStack => String -> Property ()
info msg =
    mkProperty $ \run@TestRun{ runLog = Log log } -> return (
        Right ()
      , run{ runLog = Log $ Info callStack msg : log }
      )

-- | Assert boolean
--
-- If the property is false, the test fails.
assert :: String -> Bool -> Property ()
assert _ True  = return ()
assert e False = mkProperty $ \log -> return (Left e, log)

