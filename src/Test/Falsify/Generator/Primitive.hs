-- | Primitive generator
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Generator.Primitive (PrimT)
-- > import qualified Test.Falsify.Generator.Primitive as Prim
module Test.Falsify.Generator.Primitive (
    -- * Definition
    PrimT -- opaque
    -- * Running
  , Result(..)
  , run
    -- * Specific generators
  , bytes
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Word

import Test.Falsify.Entropy (Entropy(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype PrimT m a = PrimT {
      run :: Entropy -> m (Result a)
    }
  deriving stock (Functor)

data Result a = Result {
      value  :: a
    , used   :: Entropy
    , unused :: Entropy
    }
  deriving (Functor)

{-------------------------------------------------------------------------------
  Composition

  The composition of a primitive generator is still a primitive generator: the
  structure of generator is not visible from the result (we get a used
  'Entropy', not a used 'EntropyTree'). This should therefore be used only
  sparingly.

  The 'Applicative' instance requires @Monad m@, since we need the entropy after
  running the first action to feed to the second action. There is therefore no
  reason not to define @(<*>)@ in terms of @ap@.
-------------------------------------------------------------------------------}

instance Monad m => Applicative (PrimT m) where
  pure x = PrimT $ pure . Result x mempty
  (<*>)  = ap

instance Monad m => Monad (PrimT m) where
  return  = pure
  x >>= f = PrimT $ \e -> do
      Result{value = a, used = l, unused = e'} <- run x     e
      Result{value = b, used = r, unused     } <- run (f a) e'
      return $ Result{value = b, used = l <> r, unused}

instance MonadTrans PrimT where
  lift ma = PrimT $ \e -> (\a -> Result a mempty e) <$> ma

{-------------------------------------------------------------------------------
  Specific generators
-------------------------------------------------------------------------------}

-- | Get up to @n@ bytes
--
-- Will return fewer than @n@ bytes if not enough entropy is available.
--
-- This is loosely analogue to the @getbits@ function in Hypothesis, but we
-- don't bother with bit granularity.
bytes :: Monad m => Word -> PrimT m [Word8]
bytes n = PrimT $ \(Entropy s) -> do
    let (r, s') = splitAt (fromIntegral n) s
    return $ Result {
          value  = r
        , used   = Entropy r
        , unused = Entropy s'
        }


