-- | Compound generators
module Test.Falsify.Reexported.Generator.Compound (
    -- * Compound generators
    list
    -- * Auxiliary: marking elements
  , Marked(..)
  , mark
  , shouldKeep
  , keepAtLeast
  ) where

import Control.Monad
import Data.Maybe (mapMaybe)
import Control.Monad.State

import qualified Data.Foldable as Foldable

import Test.Falsify.Internal.Generator
import Test.Falsify.Range (Range)
import Test.Falsify.Reexported.Generator.Simple

import qualified Test.Falsify.Range as Range

{-------------------------------------------------------------------------------
  Auxiliary: marking elements
-------------------------------------------------------------------------------}

data Marked a = Keep a | Drop a

-- | Mark an element, shrinking towards 'Drop'
--
-- This is an ingredient in many of the compound generators.
mark :: Gen a -> Gen (Marked a)
mark g = aux <$> bool False <*> g
  where
    aux :: Bool -> a -> Marked a
    aux False = Drop
    aux True  = Keep

shouldKeep :: Marked a -> Maybe a
shouldKeep (Keep a) = Just a
shouldKeep (Drop _) = Nothing

-- | Remark elements such that at least @n@ elements are marked 'Keep'
--
-- This does not (cannot) change the order of the elements.
keepAtLeast :: forall t a. Traversable t => Word -> t (Marked a) -> t (Marked a)
keepAtLeast n xs
  | markedAsKeep >= n = xs
  | otherwise = evalState (traverse remark xs) (n - markedAsKeep)
  where
    -- Count how many elements have already been marked as 'Keep'
    --
    -- This implies that we are doing two passes over @xs@; perhaps this can be
    -- avoided using some clever recursive program, but for now it doesn't
    -- really matter.
    --
    -- TODO: Is that really true...? After all, what if we want to lazily
    -- generate some data structure (at least the spine)? This would force the
    -- entire thing into memory? I guess this is an unavoidable of generating
    -- @n@ booleans and then looking at all of them. Probably should just be
    -- documented.
    markedAsKeep :: Word
    markedAsKeep = fromIntegral $
        length $ mapMaybe shouldKeep $ Foldable.toList xs

    remark :: Marked a -> State Word (Marked a)
    remark (Keep x) = return $ Keep x
    remark (Drop x) = state $ \m -> if m == 0
                                      then (Drop x, 0)
                                      else (Keep x, m - 1)

{-------------------------------------------------------------------------------
  Compound generators

  NOTE: An approach where we generate more elements than we need and then
  selecting some of them (possibly increasing how many we select as we "shrink")
  does NOT work: in such an approach, any value we generate but don't look at
  can trivially shrink to zero ('Minimal'); this means that if we then /do/ want
  to look at it later (because some other value would have been shrunk), we
  might only find zeroes: we want to start shrinking that only /after/ we start
  looking at it, otherwise shrinking means nothing.
-------------------------------------------------------------------------------}

-- | Generate list of specified length
list :: Range Word -> Gen a -> Gen [a]
list len g = do
    -- We do /NOT/ mark this call to 'integral' as 'withoutShrinking': it could
    -- shrink towards larger values, in which case we really need to generate
    -- more elements. This doesn't really have any downsides: it merely means
    -- that we would prefer to shrink towards a prefix of the list first, before
    -- we try to drop random other elements from the list.
    --
    -- If we have an expression such as @(,) <$> list .. <*> list@, the two
    -- lists will be shrunk independently from each other due to the branching
    -- point above them. Hence, it doesn't matter if first generator uses "fewer
    -- samples" as it shrinks.
    --
    -- After we have a list of @n@ elements, we can then drop arbitrary elements
    -- from that list, but of course doing so well only /decrease/ the list
    -- length: if @Range.origin len > n@, then we should not drop anything (in
    -- that case, the only way we can get more elements is by "shrinking" @n@
    -- towards a larger number).
    n <- integral len
    mapMaybe shouldKeep . keepAtLeast (Range.origin len) <$>
      replicateM (fromIntegral n) (mark g)


