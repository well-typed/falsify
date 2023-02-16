module Data.Falsify.List (
    -- * Predicates
    pairwiseAll
  , pairwiseAny
  ) where

{-------------------------------------------------------------------------------
  Predicates
-------------------------------------------------------------------------------}

pairwiseAll :: forall a. (a -> a -> Bool) -> [a] -> Bool
pairwiseAll p = go
  where
    go :: [a] -> Bool
    go []       = True
    go [_]      = True
    go (x:y:zs) = p x y && go (y:zs)

pairwiseAny :: forall a. (a -> a -> Bool) -> [a] -> Bool
pairwiseAny p = go
  where
    go :: [a] -> Bool
    go []       = False
    go [_]      = False
    go (x:y:zs) = p x y || go (y:zs)

