-- | Compound generators
module Test.Falsify.Reexported.Generator.Compound (
    list
  ) where

import Control.Monad
import Data.Default

import Test.Falsify.Internal.Generator
import Test.Falsify.Reexported.Generator.Simple

import qualified Test.Falsify.Range as Range

-- | Generate list of specified length
--
-- Shrinks towards shorter lists.
list :: forall a. (Word, Word) -> Gen a -> Gen [a]
list len@(lo, _) gen = do
    -- Don't shrink the length we choose.
    --
    -- By not shrinking the initial length, the generator plays a bit more
    -- nicely in composition: we can drop random elements from the list anyway
    -- (see @aux@, below), and by not shrinking this initial value after
    -- generating it ensures that the generator will always use the same amount
    -- of samples, thereby avoiding that we reuse samples for other purposes,
    -- resulting in more comprehensible shrinking behaviour.
    --
    -- This also means that the precision we use to generate this length is not
    -- relevant; this precision will affect shrinking performance, but we don't
    -- shrink.
    n <- withoutShrinking $ integral (Range.originAtLo len)
    dropMarked <$> replicateM (fromIntegral n) ((,) <$> bool def <*> gen)
  where
    -- Keep only elements marked as to keep, but respect lower bound
    -- NOTE: The bool will shrink towards 'False', thereby shrinking the list.
    dropMarked :: [(Bool, a)] -> [a]
    dropMarked elems =
        case splitAt (fromIntegral lo) elems of
          (required, optional) -> map snd $ required ++ filter fst optional
