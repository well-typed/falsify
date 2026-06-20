-- | Context
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Internal.Context (Context(Context))
-- > import qualified Test.Falsify.Internal.Context as Context
module Test.Falsify.Internal.Context (
    -- * Context
    Context(..)
  ) where

-- | Contextual data for a single test case
--
-- Properties can access contextual data pertaining to the current test case in
-- a test run. An example of its use is varying the range of generated values
-- depending on how many tests we have already done, generating a smaller range
-- in the beginning but widening the range later on.
--
-- All tests are initially run with 'finalShrink' set @False@. When we stop
-- shrinking, the final valid counterexample will be rerun with @finalShrink@
-- set @True@. It is not allowed to have this parameter affect the outcome of
-- the test. Note that in uncommon cases, the test case might not actually be a
-- shrink ('thisShrink' = @Nothing@), either because we could not shrink at all
-- or because 'maxShrinks' is @Just 0@.
--
-- Fields labeled /constant/ are the same for all tests in the test run, and
-- come from t'Test.Falsify.Driver.Options'.
data Context = Context {
      -- | Number of test cases to generate (constant)
      tests :: Word

      -- | Number of current test case, 1-based
    , thisTest :: Word

      -- | Number of shrinks allowed before failing a test (constant)
    , maxShrinks :: Maybe Word

      -- | Number of current shrink, 1-based
      --
      -- @Nothing@ when we're not currently shrinking
    , thisShrink :: Maybe Word

      -- | Is this the final counterexample after shrinking?
    , finalShrink :: Bool

      -- | Maximum number of discarded tests per successful test (constant)
    , maxRatio :: Word
    }
  deriving (Show)
