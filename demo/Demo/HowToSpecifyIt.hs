-- | Examples from "How to Specify It!: A Guide to Writing Properties of Pure
-- Functions", John Hughes, 2020, LNCS 12053.
module Demo.HowToSpecifyIt (tests) where

import Control.Monad.State
import Data.Default
import Data.List (sort)
import Data.Vector (Vector, (!))
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.Falsify

import qualified Data.Vector as V

import Data.Falsify.Tree (Tree)

import qualified Data.Falsify.Tree      as Tree
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range

tests :: TestTree
tests = testGroup "Demo.HowToSpecifyIt" [
      testGroup "Section2" [
          testProperty
            "reverse_reverse" prop_reverse_reverse
        , testPropertyWith (def { expectFailure = ExpectFailure })
            "reverse_id"      prop_reverse_id
        ]
    , testGroup "Section4" [
          testGroup "Validity" [
              testProperty "valid_nil"    prop_valid_nil
            , testProperty "valid_insert" prop_valid_insert
            , testProperty "valid_delete" prop_valid_delete
            , testProperty "valid_union"  prop_valid_union
            ]
        , testGroup "TestYourTests" [
              testProperty "valid_gen" prop_valid_gen
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Section 2: "A Primer in Property-Based Testing"
-------------------------------------------------------------------------------}

forAllLists :: ([Int] -> Property a) -> Property a
forAllLists p = do
    xs <- gen $ Gen.list (Range.between (0, 100)) $
            Gen.enum (Range.between (0, 100))
    p xs

prop_reverse_reverse :: Property ()
prop_reverse_reverse = forAllLists $ \xs -> do
    assert (show xs) $ reverse (reverse xs) == xs

prop_reverse_id :: Property ()
prop_reverse_id = forAllLists $ \xs -> do
    assert (show xs) $ reverse xs == xs

{-------------------------------------------------------------------------------
  Section 3: "Our Running Example: Binary Search Trees"
-------------------------------------------------------------------------------}

data BST k v = Leaf | Branch (BST k v) k v (BST k v)
  deriving (Eq, Show, Generic)

_find :: forall k v. Ord k => k -> BST k v -> Maybe v
_find k' = go
  where
    go :: BST k v -> Maybe v
    go (Branch l k v r)
      | k == k'   = Just v
      | k <  k'   = go l
      | otherwise = go r
    go Leaf       = Nothing

nil :: BST k v
nil = Leaf

insert :: forall k v. Ord k => k -> v -> BST k v -> BST k v
insert k' v' = go
  where
    go :: BST k v -> BST k v
    go (Branch l k v r)
      | k == k'   = Branch     l  k  v'     r
      | k' < k    = Branch (go l) k  v      r
      | otherwise = Branch     l  k  v  (go r)
    go Leaf       = Branch Leaf   k' v' Leaf

delete :: forall k v. Ord k => k -> BST k v -> BST k v
delete k' = go
  where
    go :: BST k v -> BST k v
    go (Branch l k v r)
      | k == k'   = union      l          r
      | k' < k    = Branch (go l) k v     r
      | otherwise = Branch     l  k v (go r)
    go Leaf       = Leaf

union :: forall k v. Ord k => BST k v -> BST k v -> BST k v
union = \l r -> fromAscList $ merge (toList l) (toList r)
  where
    merge :: [(k, v)] -> [(k, v)] -> [(k, v)]
    merge ((k1, v1):xs) ((k2, v2):ys)
      | k1 == k2   = (k1, v1) : merge           xs            ys -- left biased
      | k1 <  k2   = (k1, v1) : merge           xs  ((k2, v2):ys)
      | otherwise  = (k2, v2) : merge ((k1, v2):xs)           ys
    merge xs []    = xs
    merge [] ys    = ys

-- | All values in the tree, in ascending order
toList :: BST k v -> [(k, v)]
toList = go
  where
    go :: BST k v -> [(k, v)]
    go Leaf             = []
    go (Branch l k v r) = go l ++ [(k, v)] ++ go r

keys :: BST k v -> [k]
keys = map fst . toList

fromAscList :: forall k v. Ord k => [(k, v)] -> BST k v
fromAscList = \xs ->
    if sort (map fst xs) == map fst xs
      then go $ V.fromList xs
      else error "fromAscList: precondition violated"
  where
    go :: Vector (k, v) -> BST k v
    go xs
      | V.length xs == 0
      = Leaf

      | V.length xs == 1
      = let (k, v) = xs ! 0
        in Branch Leaf k v Leaf

      | otherwise
      = let mid    = V.length xs `div` 2
            szLo   = mid
            szHi   = V.length xs - 1 - szLo
            (k, v) = xs ! mid
        in Branch (go $ V.slice 0 szLo xs) k v (go $ V.slice (succ mid) szHi xs)

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

genBST :: forall k v. Num k => Gen k -> Gen v -> Gen (BST k v)
genBST = \k v ->
    flip evalState 0 . fromTree <$>
      Gen.tree (Range.between (0, 100)) ((,) <$> k <*> v)
  where
    fromTree :: Tree (k, v) -> State k (BST k v)
    fromTree Tree.Leaf = return Leaf
    fromTree (Tree.Branch (k, v) l r) = do
        l' <- fromTree l
        k' <- state $ \maxSoFar -> let k' = maxSoFar + k + 1 in (k', k')
        r' <- fromTree r
        return $ Branch l' k' v r'

genKey :: Gen Int
genKey = Gen.integral $ Range.between (0, 100)

genValue :: Gen Int
genValue = Gen.integral $ Range.between (0, 100)

forAllBST :: (BST Int Int -> Property a) -> Property a
forAllBST p = gen (genBST genKey genValue) >>= p

{-------------------------------------------------------------------------------
  Section 4: "Approaches to Writing Properties"
-------------------------------------------------------------------------------}

-- Section 4.1: "Validity Testing"

valid :: forall k v. Ord k => BST k v -> Bool
valid Leaf             = True
valid (Branch l k _ r) = and [
      valid l
    , valid r
    , all (< k) (keys l)
    , all (> k) (keys r)
    ]

-- Fig 3: Validity properties

prop_valid_nil :: Property ()
prop_valid_nil = assert "" $ valid (nil :: BST Int Int)

prop_valid_insert :: Property ()
prop_valid_insert = forAllBST $ \t -> do
    k <- gen genKey
    v <- gen genValue
    assert (show (t, k, v, insert k v t)) $ valid (insert k v t)

prop_valid_delete :: Property ()
prop_valid_delete = forAllBST $ \t -> do
    k <- gen genKey
    assert (show (t, k, delete k t)) $ valid (delete k t)

prop_valid_union :: Property ()
prop_valid_union = forAllBST $ \t -> forAllBST $ \t' ->
    assert (show (t, t', union t t')) $ valid (union t t')

-- Test your tests

prop_valid_gen :: Property ()
prop_valid_gen = forAllBST $ \t ->
    assert (show t) $ valid t
