-- | Fixed precision generators
module Test.Falsify.Internal.Generator.Precision (
    wordN
  , properFraction
  ) where

import Prelude hiding (properFraction)

import GHC.Stack

import Data.Falsify.ProperFraction (ProperFraction)
import Data.Falsify.WordN (WordN)
import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Search

import qualified Data.Falsify.WordN      as WordN
import qualified Test.Falsify.SampleTree as SampleTree

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Uniform selection of @n@-bit word of given precision, shrinking towards 0
wordN :: WordN.Precision -> Gen WordN
wordN p =
    fmap (WordN.truncateAt p . SampleTree.sampleValue) . primWith $
        binarySearch
      . WordN.forgetPrecision
      . WordN.truncateAt p
      . SampleTree.sampleValue

-- | Uniform selection of fraction, shrinking towards 0
--
-- Precondition: precision must be at least 1 bit (a zero-bit number is constant
-- 0; it is meaningless to have a fraction in a point range).
properFraction :: HasCallStack => WordN.Precision -> Gen ProperFraction
properFraction (WordN.Precision 0) = error "fraction: 0 precision"
properFraction p                   = WordN.toProperFraction <$> wordN p
