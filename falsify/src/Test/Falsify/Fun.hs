-- | Random functions
--
-- Intended for unqualified import.
--
-- > import Test.Falsify.Fun
module Test.Falsify.Fun (
    Fun(..)
  , applyFun
  , applyFun2
  , applyFun3
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
  ) where

import Data.Falsify.ConcreteFun ((:->)(..))

import qualified Data.Falsify.ConcreteFun as ConcreteFun

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Function @a -> b@ which can be shown, generated, and shrunk
data Fun a b = Fun {
      concrete      :: a :-> b
    , defaultValue  :: b

      -- Since functions are typically infinite, they can only safely be shown
      -- once they are fully shrunk: after all, once a function has been fully
      -- shrunk, we /know/ it must be finite, because in any given property, a
      -- function will only ever be applied a finite number of times.
    , isFullyShrunk :: Bool
    }
  deriving (Functor)

{-------------------------------------------------------------------------------
  Show functions
-------------------------------------------------------------------------------}

instance (Show a, Show b) => Show (Fun a b) where
  show Fun{concrete, defaultValue, isFullyShrunk}
    | isFullyShrunk = ConcreteFun.render concrete defaultValue
    | otherwise     = "<fun>"

{-------------------------------------------------------------------------------
  Patterns

  These are analogue to their counterparts in QuickCheck.
-------------------------------------------------------------------------------}

-- | Apply function to argument
--
-- See also the 'Fn', 'Fn2', and 'Fn3' patter synonyms.
applyFun :: Fun a b -> a -> b
applyFun Fun{concrete, defaultValue} = ConcreteFun.apply concrete defaultValue

-- | Like 'applyFun', but for binary functions
applyFun2 :: Fun (a, b) c -> (a -> b -> c)
applyFun2 f a b = applyFun f (a, b)

-- | Like 'applyFun', but for ternary functions
applyFun3 :: Fun (a, b, c) d -> (a -> b -> c -> d)
applyFun3 f a b c = applyFun f (a, b, c)

-- | Pattern synonym useful when generating functions of one argument
pattern Fn :: (a -> b) -> Fun a b
pattern Fn f <- (applyFun -> f)

-- | Pattern synonym useful when generating functions of two arguments
pattern Fn2 :: (a -> b -> c) -> Fun (a, b) c
pattern Fn2 f <- (applyFun2 -> f)

-- | Pattern synonym useful when generating functions of three arguments
pattern Fn3 :: (a -> b -> c -> d) -> Fun (a, b, c) d
pattern Fn3 f <- (applyFun3 -> f)

{-# COMPLETE Fn  #-}
{-# COMPLETE Fn2 #-}
{-# COMPLETE Fn3 #-}
