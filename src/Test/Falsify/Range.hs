-- | Range
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Range (Range(..))
-- > import qualified Test.Falsify.Range as Range
module Test.Falsify.Range (
    -- * Definition
    Range(..)
  , origin
    -- * Combinators
  , map
  , fromIntegral
  , fromEnum
    -- * Constructing ranges
  , full
  , between
  , num
    -- * Modifying ranges
  , invert
    -- * Queries
  , same
  ) where

import Prelude hiding (map, fromIntegral, fromEnum)
import qualified Prelude

import GHC.Stack

import Test.Falsify.Nudge

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Generator range
--
-- This type is a bit abstract. In the simplest case, we have a range with a
-- lower bound and an upper bound, which shrinks towards the lower bound:
--
-- >   <---------
-- > [*          ]
-- > lo         hi
--
-- We want to be able to 'invert' such a range to instead get
--
-- >  --------->
-- > [          *]
-- > lo         hi
--
-- In this simple case, where the origin is at one of the extreme ends of the
-- range, we can think of a \"normal\" range as one that shrinks towards 'lo'
-- and an \"inverted\" range as one that shrinks towards 'hi'.
--
-- In the general case, however, the origin is not at either end of the range,
-- but somewhere in the middle:
--
-- >  offset
-- >  -----> <------------
-- > [      *             ]
-- > lo                  hi
--
-- In this case, the inverted range is
--
-- >                offset
-- >  ------------> <-----
-- > [             *      ]
-- > lo                  hi
--
-- If all of these values were numbers, we could compute this new origin as
--
-- > hi - (origin - lo)
--
-- However, we do not want to restrict ourselves to numbers. We therefore
-- generalize, and instead define a range to consist of a lower and upper bound,
-- along with an offset and a boolean indicating whether the range has been
-- inverted or not. If not inverted, the offset is interpreted as an increase
-- from 'lo'; otherwise, the offset is interpreted as a decrease from 'hi'. The
-- precise meaning of \"increase\" or \"decrease\" is left abstract, and defined
-- by the 'Nudge' class.
data Range a = forall o. (NudgeBy o a, Show o) => Range {
      lo       :: a
    , hi       :: a
    , offset   :: o
    , inverted :: Bool
    }

deriving stock instance Show a => Show (Range a)

origin :: Range a -> a
origin Range{lo, hi, offset, inverted} =
    if not inverted
      then nudgeUp   offset lo
      else nudgeDown offset hi

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

map ::
     (NudgeBy o' b, Show o')
  => (a -> b)
  -> (forall o. NudgeBy o a => o -> o')
  -> Range a -> Range b
map f g Range{lo, hi, offset, inverted} = Range{
      lo = f lo
    , hi = f hi
    , offset = g offset
    , inverted
    }

fromIntegral :: forall a b. (Integral a, Num b, Show b) => Range a -> Range b
fromIntegral = map conv $ \o -> Offset (conv $ nudgeUp o (0 :: a))
  where
    conv :: a -> b
    conv = Prelude.fromIntegral

fromEnum :: forall a b. (Enum a, Num b, Show b) => Range a -> Range b
fromEnum = map conv $ \o -> Offset (conv $ nudgeUp o (toEnum 0 :: a))
  where
    conv :: a -> b
    conv =
          fromInteger
        . (Prelude.fromIntegral :: Int -> Integer)
        . Prelude.fromEnum

{-------------------------------------------------------------------------------
  Constructing ranges
-------------------------------------------------------------------------------}

-- | Anywhere in the range of @a@
full :: (Ord a, Bounded a) => Range a
full = between (minBound, maxBound)

-- | Inclusive range @(x, y)@ (or @(y, x)@ if @x > y@), shrinking towards @x@
between :: Ord a => (a, a) -> Range a
between (x, y)
  | x <= y    = Range{lo = x, hi = y, offset = NoOffset, inverted = False}
  | otherwise = Range{lo = y, hi = x, offset = NoOffset, inverted = True}

-- | Construct numeric range between specified bounds and with the given origin
--
-- The origin must lie within the bounds.
num :: forall a. (Integral a, HasCallStack, Show a) => (a, a) -> a -> Range a
num bounds@(x, y) o
  | x < y     = aux x y
  | otherwise = aux y x
  where
    aux :: a -> a -> Range a
    aux lo hi
      | lo <= o && o <= hi
      = let offset :: Offset a
            offset = Offset $ o - lo
        in Range{lo, hi, offset, inverted = False}

      | otherwise
      = error originNotInBounds

    originNotInBounds :: String
    originNotInBounds = concat [
         "num: origin "
        , show o
        , " not within bounds "
        , show bounds
        ]

{-------------------------------------------------------------------------------
  Modifying ranges
-------------------------------------------------------------------------------}

-- | Invert range
--
-- See 'Range' for detailed discussion.
invert :: Range a -> Range a
invert r = r{ inverted = not (inverted r) }

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Do two 'Range's denote the same range?
--
-- This does not mean that they are syntactically the same; the same range
-- can be represented in more than one way.
same :: Eq a => Range a -> Range a -> Bool
same r r' = and [
      lo     r == lo     r'
    , hi     r == hi     r'
    , origin r == origin r'
    ]