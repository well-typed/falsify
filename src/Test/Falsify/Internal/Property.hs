{-# LANGUAGE CPP #-}

-- | Properties
--
-- Intended for unqualified import.
module Test.Falsify.Internal.Property (
    -- * Property
    Property -- opaque
  , Aborted(..)
  , runProperty
    -- * State
  , TestRun(..)
  , Log(..)
  , LogEntry(..)
    -- * Running generators
  , gen
  , genWith
    -- * Other 'Property' features
  , info
  , assert
  , discard
    -- * Test shrinking
  , testShrinking
  ) where

import Prelude hiding (log)

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (toList)
import GHC.Stack

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

import Test.Falsify.Generator (Gen)
import Test.Falsify.Predicate (Predicate, (.$))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as Predicate
import qualified Test.Falsify.Predicate as P

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Property
--
-- A 'Property' is a generator that can fail and keeps a track of some
-- information about the test run.
newtype Property a = Property {
    unwrapProperty :: ExceptT Aborted (StateT TestRun Gen) a
  }
  deriving newtype (Functor, Applicative, Monad)

-- | Reason a test was aborted
data Aborted =
    TestFailed String
  | Discarded

instance MonadFail Property where
  fail = throwError

instance MonadError String Property where
  throwError = Property . throwError . TestFailed

  catchError :: forall a. Property a -> (String -> Property a) -> Property a
  prop `catchError` handler = Property $
      unwrapProperty prop `catchError` handler'
    where
      handler' :: Aborted -> ExceptT Aborted (StateT TestRun Gen) a
      handler' Discarded      = throwError Discarded
      handler' (TestFailed e) = unwrapProperty $ handler e

-- | The 'Alternative' instance uses 'discard' for 'empty'.
--
-- This means that @<|>@ provides an alternative for /discarded/ tests, not
-- for /failed/ tests.
instance Alternative Property where
  empty = discard

  (<|>) :: forall a. Property a -> Property a -> Property a
  prop <|> handler = Property $
      unwrapProperty prop `catchError` handler'
    where
      handler' :: Aborted -> ExceptT Aborted (StateT TestRun Gen) a
      handler' Discarded      = unwrapProperty handler
      handler' (TestFailed e) = throwError (TestFailed e)

-- | Construct property
--
-- This is a low-level function for internal use only.
mkProperty ::
     (TestRun -> Gen (Either Aborted a, TestRun))
  -> Property a
mkProperty = Property . ExceptT . StateT

-- | Run property
runProperty :: Property a -> Gen (Either Aborted a, TestRun)
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
  | Info String
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
  Running generators
-------------------------------------------------------------------------------}

-- | Generate value and add it to the log
gen :: (HasCallStack, Show a) => Gen a -> Property a
gen = genWithCallStack callStack (Just . show)

-- | Generalization of 'gen' that doesn't depend on a 'Show' instance
--
-- No log entry is added if 'Nothing'.
genWith :: HasCallStack => (a -> Maybe String) -> Gen a -> Property a
genWith = genWithCallStack callStack

-- | Internal auxiliary
genWithCallStack :: forall a.
     CallStack           -- ^ Explicit argument to avoid irrelevant entries
                         -- (users don't care that 'gen' uses 'genWith').
  -> (a -> Maybe String) -- ^ Entry to add to the log (if any)
  -> Gen a -> Property a
genWithCallStack stack f g = mkProperty $ \run -> aux run <$> g
  where
    aux :: TestRun -> a -> (Either Aborted a, TestRun)
    aux run@TestRun{runLog = Log log} x = (
          Right x
        , run{ runLog = Log $ case f x of
                 Just entry -> Generated stack entry : log
                 Nothing    -> log
             , runDeterministic = False
             }
        )

{-------------------------------------------------------------------------------
  Other 'Property' features
-------------------------------------------------------------------------------}

-- | Log some additional information about the test
--
-- This will be shown in verbose mode.
info :: String -> Property ()
info msg =
    mkProperty $ \run@TestRun{runLog = Log log} -> return (
        Right ()
      , run{runLog = Log $ Info msg : log}
      )

-- | Assert predicate
assert :: Predicate '[] -> Property ()
assert p =
    case Predicate.eval p of
      Left err -> throwError err
      Right () -> return ()

-- | Discard this test
discard :: Property a
discard = Property $ throwError Discarded

{-------------------------------------------------------------------------------
  Test shrinking
-------------------------------------------------------------------------------}

appendLog :: Log -> Property ()
appendLog (Log log') = mkProperty $ \run@TestRun{runLog = Log log} -> return (
      Right ()
    , run{runLog = Log $ log' ++ log}
    )

-- | Test shrinking
--
-- The property under test is not expected to fail; if it does, the resulting
-- property fails, also.
testShrinking :: forall a. Show a => Predicate [a, a] -> Property a -> Property ()
testShrinking p prop = do
    st <- genWith (const Nothing) $ Gen.toShrinkTree (runProperty prop)
    xs <- genWith (const Nothing) $ Gen.pathAny st
    case findCounterExample (toList xs) of
      Left e ->
        Property $ throwError e
      Right Nothing ->
        return ()
      Right (Just (errMsg, logBefore, logAfter)) -> do
        info "Before shrinking:"
        appendLog logBefore
        info "After shrinking:"
        appendLog logAfter
        throwError $ errMsg
  where
    findCounterExample ::
         [(Either Aborted a, TestRun)]
      -> Either Aborted (Maybe (String, Log, Log))
    findCounterExample = \case
        []                                         -> Right Nothing
        [_]                                        -> Right Nothing
        (Left e, _)     :     _                    -> Left e
        _               :     (Left e, _)     : _  -> Left e
        (Right x, logX) : ys@((Right y, logY) : _) ->
          case P.eval $ p .$ ("original", x) .$ ("shrunk", y) of
            Left err -> Right $ Just (err, runLog logX, runLog logY)
            Right () -> findCounterExample ys
