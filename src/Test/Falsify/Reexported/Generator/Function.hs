module Test.Falsify.Reexported.Generator.Function (
    Fun -- opaque
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
  , fun
  ) where

import qualified Data.Tree as Rose

import Test.Falsify.Generator.Auxiliary
import Test.Falsify.Internal.Generator
import Test.Falsify.Internal.Generator.ShrinkStep (Step)
import Test.Falsify.Reexported.Generator.Function.Perturb
import Test.Falsify.Reexported.Generator.Function.Reified
import Test.Falsify.SampleTree (SampleTree)

import qualified Test.Falsify.Internal.Generator.ShrinkStep as Step

{-------------------------------------------------------------------------------
  Functions that can be shrunk and shown

  This is the public facing API.
-------------------------------------------------------------------------------}

data Fun a b = Fun (a :-> b, b, IsFullyShrunk) (a -> b)
  deriving (Functor)

instance (Show a, Show b) => Show (Fun a b) where
  show (Fun (_, _, NotFullyShrunk) _) = "<fun>"
  show (Fun (p, d, FullyShrunk)    _) = showFunction p (Just d)

-- | Internal marker: has the function been fully shrunk?
--
-- Since functions are typically infinite, they can only safely be shown once
-- they are fully shrunk: after all, once a function has been fully shrunk,
-- we /know/ it must be finite, because in any given property, a function will
-- only ever be applied a finite number of times.
data IsFullyShrunk = FullyShrunk | NotFullyShrunk

{-------------------------------------------------------------------------------
  Patterns

  These are analogue to their counterparts in QuickCheck.
-------------------------------------------------------------------------------}

pattern Fn :: (a -> b) -> Fun a b
pattern Fn f <- (applyFun -> f)

pattern Fn2 :: (a -> b -> c) -> Fun (a, b) c
pattern Fn2 f <- (applyFun2 -> f)

pattern Fn3 :: (a -> b -> c -> d) -> Fun (a, b, c) d
pattern Fn3 f <- (applyFun3 -> f)

applyFun :: Fun a b -> (a -> b)
applyFun (Fun _ f) = f

applyFun2 :: Fun (a, b) c -> (a -> b -> c)
applyFun2 f a b = applyFun f (a, b)

applyFun3 :: Fun (a, b, c) d -> (a -> b -> c -> d)
applyFun3 f a b c = applyFun f (a, b, c)

{-# COMPLETE Fn  #-}
{-# COMPLETE Fn2 #-}
{-# COMPLETE Fn3 #-}

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

fun :: forall a b. (Function a, Perturb a) => Gen b -> Gen (Fun a b)
fun gen = do
    def <- gen
    st  <- captureLocalTree (const [])

    let f :: a -> b
        f a = run gen $ getAtFocus (perturb a) st

    uncurry (aux def) <$> fromShrinkTree (shrinkTreeReified gen st (function f))
  where
    aux :: b -> IsFullyShrunk -> (a :-> b) -> Fun a b
    aux def isFullyShrunk reified =
        Fun (reified, def, isFullyShrunk)
            (abstract reified def)

-- | Shrink reified function
--
-- Also returns whether the function has been fully shrunk. Like QuickCheck, we
-- rely on shrinking order to set this flag.
shrinkTreeReified :: forall a b.
     Gen b
  -> SampleTree
  -> (a :-> b) -> Rose.Tree (IsFullyShrunk, a :-> b)
shrinkTreeReified gen = \st f ->
    markFullyShrunk $ Rose.unfoldTree aux (f, st)
  where
    aux :: (a :-> b, SampleTree) -> (a :-> b, [(a :-> b, SampleTree)])
    aux (f, st) = (f, Step.step (shrinkReified stepGen f) st)

    -- We do not need the old value generated in order to shrink it: regular
    -- shrinking does not proceed by looking at previous values, but rather by
    -- re-running the generator on a shrunk sample tree.
    stepGen :: b -> Step b
    stepGen _ = Step.sampleTree Step.shortcutMinimal gen

    -- Add new leaves into the tree that mark the function as fully shrunk
    markFullyShrunk :: Rose.Tree (a :-> b) -> Rose.Tree (IsFullyShrunk, a :-> b)
    markFullyShrunk (Rose.Node f fs) =
        Rose.Node (NotFullyShrunk, f) $ concat [
            map markFullyShrunk fs
          , [Rose.Node (FullyShrunk, f) []]
          ]

{-------------------------------------------------------------------------------
  Shrinking reified functions
-------------------------------------------------------------------------------}

-- | Shrink a pair @(a, b)@ belonging to the graph of a function
shrinkFnPair :: forall a b. Perturb a => (b -> Step b) -> (a, b) -> Step (a, b)
shrinkFnPair step (a, b) = (a,) <$> stepAtFocus (perturb a) (step b)

-- | Shrink a reified function
--
-- This is the centrepiece of this whole module. We follow a similar strategy
-- as QuickCheck does, but the details are different. In particular, the
-- QuickCheck version depends on a shrinker for the result of the function,
-- which of course we not have: we must shrink sample trees instead.
shrinkReified :: forall a c. (c -> Step c) -> (a :-> c) -> Step (a :-> c)
shrinkReified step = go
  where
    -- When we generate a reified function it will typically be infinitely
    -- large. It is therefore critical that we can replace entire chunks of the
    -- concrete function with 'Nil', so that shrinking will terminate (this is
    -- very similar to replacing entire parts of the sample tree with Minimal).
    go :: forall x. (x :-> c) -> Step (x :-> c)
    go Nil = go' Nil
    go f   = go' f `Step.butPrefer` [Nil]

    go' :: forall x. (x :-> c) -> Step (x :-> c)
    go' Nil         = mempty
    go' (Unit c)    = Unit      <$> step c
    go' (Map f g p) = mkMap f g <$> go p
    go' (Prod f)    = mkProd    <$> shrinkReified go f
    go' (Sum f g)   = mconcat [
                          (\f' -> mkSum f' g ) <$> go f
                        , (\g' -> mkSum f  g') <$> go g
                        ]
    go' (Table xys) = mkTable   <$> (   Step.one (shrinkFnPair step) xys
                                      `Step.butPrefer`
                                        removeSome xys
                                    )

{-------------------------------------------------------------------------------
  Internal auxiliary: sample-tree independent shrinking

  This is adapted from code in QuickCheck.
-------------------------------------------------------------------------------}

removeSome :: [a] -> [[a]]
removeSome xs = concat [
     -- remove all, half, 1/4th, .. of all elements
     concat [
          removeChunkOfSize k n xs
        | k <- takeWhile (> 0) (iterate (`div` 2) n)
        ]
   ]
 where
   n = length xs

-- | All ways to remove @k@ consecutive elements from a list
removeChunkOfSize ::
     Int  -- ^ Size of the chunks to remove
  -> Int  -- ^ Total length of the list
  -> [a] -> [[a]]
removeChunkOfSize k = go
  where
    go ::
         Int  -- Remaining length of the list
      -> [a] -> [[a]]
    go n xs
      | k > n     = []   -- we need to remove more elements than we have left
      | null xs2  = [[]] -- we need to remove all elements
      | otherwise = xs2 : map (xs1 ++) (go (n - k) xs2)
      where
        (xs1, xs2) = splitAt k xs
