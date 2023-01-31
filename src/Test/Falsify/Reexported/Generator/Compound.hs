-- | Compound generators
module Test.Falsify.Reexported.Generator.Compound (
    list
  ) where

import Control.Monad
import Data.Word

import Test.Falsify.Internal.Generator
import Test.Falsify.Reexported.Generator.Simple

import qualified Test.Falsify.Range as Range

-- | Generate list of specified length
--
-- Shrinks towards shorter lists.
list :: forall a. (Word, Word) -> Gen a -> Gen [a]
list len@(lo, _) gen = do
    -- By not shrinking the initial length, the generator plays a bit more
    -- nicely in composition: we can drop random elements from the list anyway
    -- (see @aux@, below), and by not shrinking this initial value after
    -- generating it ensures that the generator will always use the same amount
    -- of samples, thereby avoiding that we reuse samples for other purposes,
    -- resulting in more comprehensible shrinking behaviour.
    n <- withoutShrinking $ integral (Range.originAtLo len)
    dropMarked <$> replicateM (fromIntegral n) (aux <$> prim <*> gen)
  where
    -- Drop the element if the first 'Word64' is zero.
    --
    -- The chance of this happening is tiny (1 in 2^64), so the initial list
    -- will have very close to the specified number of elements. However,
    -- shrinking can then reduce these "guards" to 0, thereby dropping random
    -- elements from the list.
    aux :: Word64 -> a -> (Bool, a)
    aux x a = (x == 0, a)

    -- | Drop all elements marked to be dropped, but respect lower bound
    dropMarked :: [(Bool, a)] -> [a]
    dropMarked elems =
        case splitAt (fromIntegral lo) elems of
          (required, optional) -> map snd $
             required ++ filter (not . fst) optional
