-- | Simple (i.e., non-compound) generators
module Test.Falsify.Reexported.Generator.Simple (
    bool
  , integral
  ) where

import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Nudge
import Test.Falsify.Range (Range(..), origin)

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

bool :: Range NoOffset Bool -> Gen Bool
bool Range{lo, hi, inverted} = aux <$> fraction
  where
    aux :: Fraction -> Bool
    aux (Fraction f) =
        if not inverted
          then if f < 0.5 then lo else hi
          else if f < 0.5 then hi else lo

integral :: forall o a. (NudgeBy o Double, Integral a) => Range o a -> Gen a
integral = \r -> aux (fromIntegral <$> r) <$> signedFraction
  where
    aux :: Range o Double -> Signed Fraction -> a
    aux r (Neg (Fraction f)) = round $ origin r - f * (origin r - lo r)
    aux r (Pos (Fraction f)) = round $ origin r + f * (hi r - origin r)
