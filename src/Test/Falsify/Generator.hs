-- | Generator
--
-- Intended for qualified import
--
-- > import Test.Falsify.Generator (GenT)
-- > import qualified Test.Falsify.Generator as Gen
module Test.Falsify.Generator (
    -- * Definition
    GenT -- opaque
    -- * Construction
  , bytes
  , byte
    -- * Running
  , Result(..)
  , runT
  , rerunT
    -- * Simplified interface
  , Gen
  , run
  , rerun
  ) where

import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Word

import Test.Falsify.Entropy (Entropy(..))
import Test.Falsify.Entropy.Tree (EntropyTree(L, B))
import Test.Falsify.Generator.Primitive (PrimT)

import qualified Test.Falsify.Entropy.Tree as ETree
import qualified Test.Falsify.Generator.Primitive as Prim

{-------------------------------------------------------------------------------
  Definition

  The generator is defined as a free structure. This is necessary, because we
  have two separate interpretations of the generator: one over a flat 'Entropy',
  and one over an 'EntropyTree'. In the latter case, the structure of the
  generator needs to be visible, so we can try to match it against the structure
  of the tree.

  A generator defined using the applicative interface only will always have the
  same structure regardless of input entropy, and therefore will always result
  in the same shape entropy tree; this is not true for a generator defined using
  the monadic interface.

  However, we do not need to include a special constructor corresponding to
  applicative `(<*>)`, as we can _detect_ when the tree shape is different from
  what is expected by the generator, and flatten it at that point. So if the
  monadic interface is " unnecessarily " used, in the sense that the generator
  structure does not in fact depend on the entropy, we will still be able to
  match the generator against a (previously constructed) entropy tree. This
  doesn't mean that users don't need to be aware of the difference at all:
  shrinking is more easily understood and more predictable when the structure of
  the entropy tree does not have to change.

  We define 'Lift' as its own constructor; this is important when re-running a
  generator, as this will succeed independent of the shape of the entropy tree.
  We also define 'FMap' as its own constructor; this is not strictly necessary,
  but simplifies the structure of the resulting entropy tree, which makes
  debugging (of @falsify@ itself) easier.
-------------------------------------------------------------------------------}

-- | Generator
data GenT m a where
  Prim :: PrimT m a -> GenT m a
  Lift :: m a -> GenT m a
  FMap :: (a -> b) -> GenT m a -> GenT m b
  Bind :: GenT m a -> (a -> GenT m b) -> GenT m b

instance MonadTrans GenT where
  lift = Lift

instance Applicative m => Functor (GenT m) where
  fmap = FMap

instance Applicative m => Applicative (GenT m) where
  pure    = Lift . pure
  f <*> x = Bind f (`FMap` x)

instance Applicative m => Monad (GenT m) where
  return = pure
  (>>=)  = Bind

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Ask for @n@ bytes of entropy
--
-- Fewer than @n@ bytes might be provided if not enough entropy is available.
-- All generators should be able to cope with this; in particular, the empty
-- entropy should describe the " simplest " value, for some definition of "
-- simple " which is suitable to the specific application domain.
bytes :: Monad m => Word -> GenT m [Word8]
bytes = Prim . Prim.bytes

byte :: Monad m => GenT m Word8
byte = aux <$> bytes 1
  where
    aux :: [Word8] -> Word8
    aux []    = 0
    aux (b:_) = b

{-------------------------------------------------------------------------------
  Running the generator
-------------------------------------------------------------------------------}

data Result a = Result {
      value  :: a
    , used   :: EntropyTree
    , unused :: Entropy
    }
  deriving stock (Show, Functor)

trivial :: Entropy -> a -> Result a
trivial unused value = Result{
      value
    , used = L []
    , unused
    }

fromPrim :: Prim.Result a -> Result a
fromPrim Prim.Result{value, used = Entropy e, unused} = Result{
      value
    , used = L e
    , unused
    }

-- | Run generator against flat entropy
runT :: Monad m => GenT m a -> Entropy -> m (Result a)
runT (Prim p)   e = fromPrim <$> Prim.run p e
runT (Lift p)   e = trivial e <$> p
runT (FMap f g) e = fmap f <$> runT g e
runT (Bind x f) e = do
    Result{value = a, used = l, unused = e'} <- runT x     e
    Result{value = b, used = r, unused}      <- runT (f a) e'
    return Result{value = b, used = B l r, unused}

{-------------------------------------------------------------------------------
  Re-running the generator

  We cannot simply flatten the tree, because we do not merely reduce values
  in the tree leaves, we also /drop/ values. Thus, if we were to flatten and
  then simply re-run, the structure of the tree would no longer match the
  structure of the generator. This would defeat the purpose of having an
  entropy tree in the first place.
-------------------------------------------------------------------------------}

data RerunResult a =
    -- | The generator could succeed without changing the shape of the tree
    --
    -- We nonetheless include the \"resulting\" entropy tree: this is useful
    -- to collapse entire parts of the entropy tree in the 'Lift' case.
    SameShape EntropyTree a

    -- | The generator needed a different shape tree
  | ShapeChanged (Result a)
  deriving stock (Show, Functor)

-- | Attempt to run generator against previously constructed entropy tree
--
-- When the structure of the tree does not match the structure of the generator,
-- the tree is flattened and we default to 'runT'.
rerunT :: forall m a. Monad m => GenT m a -> EntropyTree -> m (RerunResult a)
rerunT = go
  where
    go :: forall x. GenT m x -> EntropyTree -> m (RerunResult x)
    go (Lift p)    _t =  SameShape (L []) <$> p
    go (FMap f g)   t = fmap f <$> go g t
    go (Prim p) t     =
        case t of
          L e -> SameShape t  . Prim.value <$> Prim.run p (Entropy e)
          B{} -> ShapeChanged . fromPrim   <$> Prim.run p (ETree.flatten t)
    go g@(Bind x f) t =
        case t of
          L{}   -> ShapeChanged <$> runT g (ETree.flatten t)
          B l r ->
            go x l >>= \case
              SameShape l' a -> go (f a) r >>= \case
                SameShape r' b ->
                  return $ SameShape (B l' r') b
                ShapeChanged Result{value = b, used = r', unused} ->
                  return $ ShapeChanged Result{
                       value = b
                     , used  = B l' r'
                     , unused
                     }
              ShapeChanged Result{value = a, used = l', unused = e} -> do
                Result{value = b, used = r', unused} <- runT (f a) e
                return $ ShapeChanged Result{
                      value = b
                    , used  = B l' r'
                    , unused
                    }

{-------------------------------------------------------------------------------
  Simplified interface
-------------------------------------------------------------------------------}

type Gen = GenT Identity

run :: Gen a -> Entropy -> Result a
run g = runIdentity . runT g

rerun :: Gen a -> EntropyTree -> RerunResult a
rerun g = runIdentity . rerunT g
