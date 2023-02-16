-- | Properties
--
-- Intended for unqualified import.
--
-- TODO: We should split this into a public and an internal module; the 'Log'
-- stuff is strictly internal.
module Test.Falsify.Property (
    -- * Property
    Property -- opaque
  , runProperty
  , mapFailure
    -- * Log
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
-- A 'Property' is a generator that can fail and keeps a log of what happened
-- during a test run.
newtype Property e a = Property {
    unwrapProperty :: ExceptT e (StateT Log Gen) a
  }
  deriving newtype (Functor, Applicative, Monad)

-- | Construct property
--
-- This is a low-level function for internal use only.
mkProperty :: (Log -> Gen (Either e a, Log)) -> Property e a
mkProperty = Property . ExceptT . StateT

-- | Run property
runProperty :: Property e a -> Gen (Either e a, Log)
runProperty = flip runStateT (Log []) . runExceptT . unwrapProperty

mapFailure :: (e -> e') -> Property e a -> Property e' a
mapFailure f (Property p) = Property (withExceptT f p)

{-------------------------------------------------------------------------------
  Log
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Generate value and add it to the log
gen :: (HasCallStack, Show a) => Gen a -> Property e a
gen = genWith show

-- | Generalization of 'gen' that doesn't depend on a 'Show' instance
genWith :: forall e a. HasCallStack => (a -> String) -> Gen a -> Property e a
genWith f g = mkProperty $ \log -> aux log <$> g
  where
    aux :: Log -> a -> (Either e a, Log)
    aux (Log log) x = (Right x, Log $ Generated callStack (f x) : log)

-- | Log some additional information about the test
--
-- This will be shown in verbose mode.
info :: HasCallStack => String -> Property e ()
info msg =
    mkProperty $ \(Log log) ->
      return (Right (), Log $ Info callStack msg : log)

-- | Assert boolean
--
-- If the property is false, the test fails.
assert :: e -> Bool -> Property e ()
assert _ True  = return ()
assert e False = mkProperty $ \log -> return (Left e, log)
