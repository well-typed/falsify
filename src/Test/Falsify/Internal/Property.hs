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
  , assertBool
  , expect
  , assertEqual
  , assertRelatedBy
  , discard
    -- * Test shrinking
  , testShrinking
  ) where

import Prelude hiding (log)

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (toList)
import GHC.Stack

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

import Test.Falsify.Generator (Gen)

import qualified Test.Falsify.Generator as Gen
import Control.Applicative

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

-- | Assert boolean
--
-- If the property is false, the test fails.
assert :: String -> Bool -> Property ()
assert _ True  = return ()
assert e False = throwError e

-- | Like 'assert', but with a standard message.
assertBool :: Bool -> Property ()
assertBool = assert "Failed"

-- | Assert that two values are equal
--
-- See also 'assertEqual'.
expect ::
     (Eq a, Show a)
  => a  -- ^ Expected value
  -> a  -- ^ Actual value
  -> Property ()
expect x y
  | x == y    = return ()
  | otherwise = throwError $ unlines [
        "expected: " ++ show x
      , "but got:  " ++ show y
      ]

-- | Like 'expect', but without a particular towards an \"expected\" value
assertEqual :: (Eq a, Show a) => a -> a  -> Property ()
assertEqual = assertRelatedBy (==)

-- | Generalization of 'assertEqual' to arbitrary binary relation
assertRelatedBy :: Show a => (a -> a -> Bool) -> a -> a -> Property ()
assertRelatedBy p x y
  | p x y     = return ()
  | otherwise = throwError $ unlines [
        "Not related"
      , "  lhs: " ++ show x
      , "  rhs: " ++ show y
      ]

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
testShrinking :: forall a.
     Show a
  => (a -> a -> Bool) -> Property a -> Property ()
testShrinking p prop = do
    st <- genWith (const Nothing) $ Gen.toShrinkTree (runProperty prop)
    xs <- genWith (const Nothing) $ Gen.path st
    case findCounterExample (toList xs) of
      Left e ->
        Property $ throwError e
      Right Nothing ->
        return ()
      Right (Just ((x, xLog), (y, yLog))) -> do
        info "Before shrinking:"
        appendLog xLog
        info "After shrinking:"
        appendLog yLog
        throwError $ "Invalid shrink: " ++ show x ++ " ~> " ++ show y
  where
    findCounterExample ::
         [(Either Aborted a, TestRun)]
      -> Either Aborted (Maybe ((a, Log), (a, Log)))
    findCounterExample = \case
        []                                         -> Right Nothing
        [_]                                        -> Right Nothing
        (Left e, _)     :     _                    -> Left e
        _               :     (Left e, _)     : _  -> Left e
        (Right x, logX) : ys@((Right y, logY) : _) ->
          if p x y then findCounterExample ys
                   else Right $ Just ((x, runLog logX), (y, runLog logY))
