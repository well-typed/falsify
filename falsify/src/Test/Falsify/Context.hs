-- | Context
--
-- Intended for qualified import.
--
-- > import Test.Falsify
-- > import Test.Falsify.Context (Context)
-- > import qualified Test.Falsify.Context as Context
module Test.Falsify.Context (
    -- * Context
    Context(..)
  , Static(..)
  , Iteration(..)
  , Execution(..)
  ) where

-- | Contextual data for a single test case
--
-- Properties can access contextual data pertaining to the current test case in
-- a test run. An example of its use is varying the range of generated values
-- depending on how many tests we have already done, generating a smaller range
-- in the beginning but widening the range later on.
data Context = Context{
      static    :: Static
    , iteration :: Iteration
    , execution :: Execution
    }
  deriving stock (Show, Eq)

-- | Static context
--
-- The part of the context that does not change between test iterations.
data Static = Static{
      -- | Number of test cases to generate
      tests :: Word

      -- | Number of shrinks allowed before failing a test
    , maxShrinks :: Maybe Word

      -- | Maximum number of discarded tests per successful test
    , maxRatio :: Word
    }
  deriving stock (Show, Eq)

-- | Iteration context
--
-- Information about the current test iteration specifically.
data Iteration = Iteration{
      -- | Number of current test case, 0-based
      thisTest :: Word
    }
  deriving stock (Show, Eq)

-- | Test execution context
--
-- NOTE: This should not affect whether the test passes or fails; if it does,
-- you will get undefined shrinking behaviour.
data Execution =
    -- | Initial test execution
    Initial

    -- | Shrink step
    --
    -- We record the index of the shrink step (0-based)
  | Shrinking Word

    -- | Final test execution
    --
    -- We always end a test execution with one more final run, which is a repeat
    -- of the run that came just before it.
    --
    -- We record how many shrink steps we took in between 'Initial' and 'Final';
    -- this may be zero, if the initial test happened to be minimal already or
    -- shrinking is disabled.
  | Final Word
  deriving stock (Show, Eq)
