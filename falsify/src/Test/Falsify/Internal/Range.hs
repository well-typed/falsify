-- | Internal 'Range' API
module Test.Falsify.Internal.Range (
    -- * Definition
    Range(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import Data.Falsify.WordN (WordN)

import qualified Data.Falsify.WordN as WordN

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Range of values
data Range a where
  -- | Constant (point) range
  Constant :: a -> Range a

  -- | Construct value from t'WordN' of the given precision
  --
  -- This is the main constructor for 'Range'.
  --
  -- Typically this t'WordN' is used to construct a fraction which is then used
  -- to index the range (see 'Test.Falsify.Range.fromProperFraction'), though
  -- some generators use the t'WordN' directly.
  FromWordN :: WordN.Precision -> (WordN -> a) -> Range a

  -- | Evaluate each range and choose the \"smallest\"
  --
  -- Each value in the range is annotated with some distance metric; for
  -- example, this could be the distance to some predefined point (e.g. as in
  -- 'Test.Falsify.Range.towards')
  Smallest :: Ord b => NonEmpty (Range (a, b)) -> Range a

deriving stock instance Functor Range
