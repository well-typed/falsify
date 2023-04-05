module TestSuite.Util.List (
    -- * Predicates
    pairwiseAll
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
