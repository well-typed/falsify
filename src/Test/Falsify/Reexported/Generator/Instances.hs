{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Falsify.Reexported.Generator.Instances (
    -- * Re-exports
    Alt(..)
  ) where

import Control.Selective
import Data.Functor.Alt

import Test.Falsify.Internal.Generator
import Test.Falsify.Reexported.Generator.Simple

{-------------------------------------------------------------------------------
  Additional instances
-------------------------------------------------------------------------------}

-- | Generate a value with one of two generators
--
-- Shrinks towards the first generator.
--
-- In the remainder of this docstring we give some background to this instance,
-- which may be useful for general understanding of the @falsify@ library.
--
-- The implementation takes advantage of the that 'Gen' is a selective functor
-- to ensure that the two generators can shrink independently: if the initial
-- value of the generator is some @y@ produced by the second generator, later
-- shrunk to some @y'@, then if the generator can shrink to @x@ at some point,
-- produced by the /first/ generator, then shrinking effectively "starts over":
-- the value of @x@ is independent of @y'@.
--
-- That is different from doing this:
--
-- > do b <- bool
-- >    if b then l else r
--
-- In this case, @l@ and @r@ will be generated from the /same/ sample tree,
-- and so cannot shrink independently.
--
-- It is /also/ different from
--
-- > do x <- l
-- >    y <- r
-- >    b <- bool
-- >    return $ if b then x else y
--
-- In this case, @l@ and @r@ are run against /different/ sample trees, like we
-- do here, /but/ in this case if the current value produced by the generator is
-- produced by the right generator, then the sample tree used for the left
-- generator will always shrink to 'Minimal' (this /must/ be possible because
-- we're not currently using it); this means that we would then only be able to
-- shrink to a value from the left generator if the /minimal/ value produced by
-- that generator happens to work.
--
-- To rephrase that last point: generating values that are not actually used
-- will lead to poor shrinking, since those values can always be shrunk to their
-- minimal value, independently from whatever property is being tested: the
-- shrinker does not know that the value is not being used. The correct way to
-- conditionally use a value is to use the selective interface, as we do here.
instance Alt Gen where
  (<!>) :: Gen a -> Gen a -> Gen a
  (<!>) = ifS (bool True)

