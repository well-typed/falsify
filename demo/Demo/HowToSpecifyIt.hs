-- | Examples from "How to Specify It!: A Guide to Writing Properties of Pure
-- Functions", John Hughes, 2020, LNCS 12053.
module Demo.HowToSpecifyIt (tests) where

import Control.Applicative
import Control.Monad.State
import Data.Default
import Data.Function
import Data.List (sort)
import Data.Vector (Vector, (!))
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.Falsify

import qualified Data.List   as L
import qualified Data.Tree   as Rose
import qualified Data.Vector as V

import Test.Falsify.Predicate ((.$))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range
import qualified Test.Falsify.Predicate as P

tests :: TestTree
tests = testGroup "Demo.HowToSpecifyIt" [
      testGroup "Section2" [
          testProperty                   "reverse_reverse" prop_reverse_reverse
        , testPropertyWith expectFailure "reverse_id"      prop_reverse_id
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
        , testGroup "Postconditions" [
              testProperty "insert"       prop_post_insert
            , testProperty "union"        prop_post_union
            , testProperty "find_present" prop_post_find_present
            , testProperty "find_absent"  prop_post_find_absent
            ]
        , testProperty "complete_insert_delete" prop_complete_insert_delete
        , testGroup "Metamorphic" [
              testProperty "insert_insert"      prop_insert_insert
            , testProperty "insert_insert_weak" prop_insert_insert_weak
            , testProperty "insert_delete"      prop_insert_delete
            , testProperty "insert_union"       prop_insert_union
            ]
        , testGroup "PreserveEquiv" [
              testProperty "insert" prop_preserveEquiv_insert
            ]
        , testGroup "Inductive" [
              testProperty "union_nil"    prop_union_nil
            , testProperty "union_insert" prop_union_insert
            , testGroup "Completeness" [
                  testProperty "insert" prop_complete_insert
                , testProperty "delete" prop_complete_delete
                , testProperty "union"  prop_complete_union
                ]
            ]
        , testGroup "Model" [
              testProperty                   "nil"          prop_model_nil
            , testProperty                   "insert"       prop_model_insert
            , testPropertyWith expectFailure "insert_wrong" prop_model_insert_wrong
            ]
        , testGroup "Generation" [
              testPropertyWith (def { overrideNumTests = Just 10_000 })
                "measure" prop_measure
            , testPropertyWith (def { overrideNumTests = Just 10_000 })
                "measure_small" prop_measure_small
            ]
        ]
    ]
  where
    expectFailure :: TestOptions
    expectFailure = def {
          expectFailure    = ExpectFailure
        , overrideNumTests = Just 1000
        }

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
    assert $ P.eq .$ ("lhs", reverse (reverse xs))
                  .$ ("rhs", xs)

prop_reverse_id :: Property ()
prop_reverse_id = forAllLists $ \xs -> do
    assert $ P.eq .$ ("lhs", reverse xs)
                  .$ ("rhs", xs)

{-------------------------------------------------------------------------------
  Section 3: "Our Running Example: Binary Search Trees"
-------------------------------------------------------------------------------}

data BST k v = Leaf | Branch (BST k v) k v (BST k v)
  deriving (Eq, Show, Generic)

equivBST :: (Eq k, Eq v) => BST k v -> BST k v -> Bool
equivBST = (==) `on` toList

showBST :: forall k v. (Show k, Show v) => BST k v -> String
showBST = ("\n" ++) . Rose.drawTree . toRoseTree
  where
    toRoseTree :: BST k v -> Rose.Tree String
    toRoseTree Leaf             = Rose.Node "*" []
    toRoseTree (Branch l k v r) = Rose.Node (show (k, v)) [
          toRoseTree l
        , toRoseTree r
        ]

find :: forall k v. Ord k => k -> BST k v -> Maybe v
find k' = go
  where
    go :: BST k v -> Maybe v
    go (Branch l k v r)
      | k == k'   = Just v
      | k' < k    = go l
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

merge :: Ord k => [(k, v)] -> [(k, v)] -> [(k, v)]
merge ((k1, v1):xs) ((k2, v2):ys)
  | k1 == k2   = (k1, v1) : merge           xs            ys -- left biased
  | k1 <  k2   = (k1, v1) : merge           xs  ((k2, v2):ys)
  | otherwise  = (k2, v2) : merge ((k1, v1):xs)           ys
merge xs []    = xs
merge [] ys    = ys

-- | All values in the tree, in ascending order
toList :: BST k v -> [(k, v)]
toList = go
  where
    go :: BST k v -> [(k, v)]
    go Leaf             = []
    go (Branch l k v r) = go l ++ [(k, v)] ++ go r

fromList :: Ord k => [(k, v)] -> BST k v
fromList = foldr (uncurry insert) nil

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

  TODO: We should try to write a generator that shrinks more efficiently.
-------------------------------------------------------------------------------}

genBST :: forall k v. Ord k => Gen k -> Gen v -> Gen (BST k v)
genBST k v = fromList <$> Gen.list (Range.between (0, 100)) ((,) <$> k <*> v)

genKey :: Gen Int
genKey = Gen.integral $ Range.between (0, 100)

genValue :: Gen Int
genValue = Gen.integral $ Range.between (0, 100)

{-------------------------------------------------------------------------------
  Section 4: "Approaches to Writing Properties"

  Section 4.1: "Validity Testing"
-------------------------------------------------------------------------------}

valid :: forall k v. Ord k => BST k v -> Bool
valid Leaf             = True
valid (Branch l k _ r) = and [
      valid l
    , valid r
    , all (< k) (keys l)
    , all (> k) (keys r)
    ]

predValid :: Ord k => P.Predicate '[BST k v]
predValid = P.satisfies ("valid", valid)

{-------------------------------------------------------------------------------
  Fig 3: Validity properties
-------------------------------------------------------------------------------}

prop_valid_nil :: Property ()
prop_valid_nil =
    assert $ predValid .$ ("nil", nil :: BST Int Int)

prop_valid_insert :: Property ()
prop_valid_insert = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    k <- gen genKey
    v <- gen genValue
    let t' = insert k v t
    assert $ predValid .$ ("t'", t')

prop_valid_delete :: Property ()
prop_valid_delete = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    k <- gen genKey
    let t' = delete k t
    assert $ predValid .$ ("t'", t')

prop_valid_union :: Property ()
prop_valid_union = do
    t  <- genWith (Just . showBST) $ genBST genKey genValue
    t' <- genWith (Just . showBST) $ genBST genKey genValue
    let t'' = union t t'
    assert $ predValid .$ ("t''", t'')

-- Test your tests

prop_valid_gen :: Property ()
prop_valid_gen = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    assert $ predValid .$ ("t", t)

{-------------------------------------------------------------------------------
  Section 4.2 Postconditions
-------------------------------------------------------------------------------}

prop_post_insert :: Property ()
prop_post_insert = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    k  <- gen genKey
    k' <- gen genKey
    v  <- gen genValue
    let t'       = insert k v t
        expected = if k == k'
                     then Just v
                     else find k' t
    info $ "t': " ++ show t'
    assert $ P.expect expected .$ ("actual", find k' t')

prop_post_union :: Property ()
prop_post_union = do
    t  <- genWith (Just . showBST) $ genBST genKey genValue
    t' <- genWith (Just . showBST) $ genBST genKey genValue
    k  <- gen genKey
    let t''      = union t t'
        expected = find k t <|> find k t'
    info $ "toList t: " ++ show (toList t)
    info $ "toList t': " ++ show (toList t')
    info $ "merged: " ++ show (merge (toList t) (toList t'))
    info $ "t'': " ++ showBST t''
    assert $ P.expect expected .$ ("actual", find k t'')

prop_post_find_present :: Property ()
prop_post_find_present = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    k <- gen genKey
    v <- gen genValue
    assert $ P.expect (Just v) .$ ("actual", find k (insert k v t))

prop_post_find_absent :: Property ()
prop_post_find_absent = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    k <- gen genKey
    assert $ P.expect Nothing .$ ("actual", find k (delete k t))

prop_complete_insert_delete :: Property ()
prop_complete_insert_delete = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    k <- gen genKey
    case find k t of
      Nothing -> assert $ P.expect t .$ ("deleted"  , delete k   t)
      Just v  -> assert $ P.expect t .$ ("inserted" , insert k v t)

{-------------------------------------------------------------------------------
  Section 4.3 Metamorphic properties

  TODO: There are more metamorphic properties listed in the paper (Appendix A)
-------------------------------------------------------------------------------}

predEquiv :: (Eq k, Eq v) => P.Predicate '[BST k v, BST k v]
predEquiv = P.relatedBy ("equivBST", equivBST)

prop_insert_insert :: Property ()
prop_insert_insert = do
    t  <- genWith (Just . showBST) $ genBST genKey genValue
    k  <- gen genKey
    k' <- gen genKey
    v  <- gen genValue
    v' <- gen genValue

    let lhs  = insert k  v  $ insert k' v' $ t
        rhs1 = insert k' v' $ insert k  v  $ t
        rhs2 = insert k  v  $                t

    assert $
         P.choose_
           (predEquiv .$ ("rhs1", rhs1))
           (predEquiv .$ ("rhs2", rhs2))
      .$ ("differentKey", k /= k')
      .$ ("lhs", lhs)

prop_insert_insert_weak :: Property ()
prop_insert_insert_weak = do
    t  <- genWith (Just . showBST) $ genBST genKey genValue
    k  <- gen genKey
    k' <- gen genKey
    when (k == k') discard -- this is the line that makes this property "weak"
    v  <- gen genValue
    v' <- gen genValue

    let lhs = insert k  v  $ insert k' v' $ t
        rhs = insert k' v' $ insert k  v  $ t

    assert $
         predEquiv
      .$ ("lhs", lhs)
      .$ ("rhs", rhs)

prop_insert_delete :: Property ()
prop_insert_delete = do
    t  <- genWith (Just . showBST) $ genBST genKey genValue
    k  <- gen genKey
    k' <- gen genKey
    v  <- gen genValue

    let lhs  = insert k  v $ delete k'  $ t
        rhs1 = delete k'   $ insert k v $ t
        rhs2 = insert k  v                t

    assert $
         P.choose_
           (predEquiv .$ ("rhs1", rhs1))
           (predEquiv .$ ("rhs2", rhs2))
      .$ ("differentKey", k /= k')
      .$ ("lhs", lhs)

prop_insert_union :: Property ()
prop_insert_union = do
    t  <- genWith (Just . showBST) $ genBST genKey genValue
    t' <- genWith (Just . showBST) $ genBST genKey genValue
    k  <- gen genKey
    v  <- gen genValue

    let lhs = insert k v $ union t t'
        rhs = union (insert k v t) t'

    assert $
         predEquiv
      .$ ("lhs", lhs)
      .$ ("rhs", rhs)

-- Preservation of equivalence
--
-- TODO: There are more of these properties listed in the paper.

genEquivPair :: Gen (BST Int Int, BST Int Int)
genEquivPair = do
    t1  <- genBST genKey genValue
    kvs <- Gen.shuffle (toList t1)
    return (t1, fromList kvs)

showPair :: (BST Int Int, BST Int Int) -> String
showPair (t1, t2) = unlines [
      "tree 1:"
    , showBST t1
    , "tree 2:"
    , showBST t2
    ]

prop_preserveEquiv_insert :: Property ()
prop_preserveEquiv_insert = do
    (t1, t2) <- genWith (Just . showPair) genEquivPair
    k <- gen genKey
    v <- gen genValue
    assert $
         predEquiv
      .$ ("lhs", insert k v t1)
      .$ ("rhs", insert k v t2)

{-------------------------------------------------------------------------------
  4.4 Inductive Testing
-------------------------------------------------------------------------------}

prop_union_nil :: Property ()
prop_union_nil = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    assert $ predEquiv
      .$ ("lhs", union nil t)
      .$ ("rhs", t)

prop_union_insert :: Property ()
prop_union_insert = do
    t  <- genWith (Just . showBST) $ genBST genKey genValue
    t' <- genWith (Just . showBST) $ genBST genKey genValue
    k  <- gen genKey
    v  <- gen genValue

    let lhs = union (insert k v t) t'
        rhs = insert k v (union t t')

    assert $ predEquiv
      .$ ("lhs", lhs)
      .$ ("rhs", rhs)

insertions :: BST k v -> [(k, v)]
insertions Leaf = []
insertions (Branch l k v r) = (k, v) : insertions l ++ insertions r

prop_complete ::
     (Show k, Show v, Ord k, Ord v)
  => BST k v -> Property' String ()
prop_complete t =
    assert $ P.eq -- we really want equality here, not equivalence
      .$ ("lhs", t)
      .$ ("rhs", foldl (flip $ uncurry insert) nil (insertions t))

prop_complete_insert :: Property ()
prop_complete_insert = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    prop_complete t

prop_complete_delete :: Property ()
prop_complete_delete = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    k <- gen genKey
    prop_complete (delete k t)

prop_complete_union :: Property ()
prop_complete_union = do
    t  <- genWith (Just . showBST) $ genBST genKey genValue
    t' <- genWith (Just . showBST) $ genBST genKey genValue
    prop_complete (union t t')

{-------------------------------------------------------------------------------
  Section 4.5 Model-based properties

  TODO: There a a few more properties listed in the paper.
-------------------------------------------------------------------------------}

deleteKey :: Eq k => k -> [(k, b)] -> [(k, b)]
deleteKey k = filter ((/= k) . fst)

prop_model_nil :: Property ()
prop_model_nil =
    assert $ P.eq
      .$ ("lhs", toList (nil :: BST Int Int))
      .$ ("rhs", [])

prop_model_insert :: Property ()
prop_model_insert = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    k <- gen genKey
    v <- gen genValue
    assert $ P.eq
      .$ ("lhs", toList (insert k v t))
      .$ ("rhs", L.insert (k, v) (deleteKey k $ toList t))

prop_model_insert_wrong :: Property ()
prop_model_insert_wrong = do
    t <- genWith (Just . showBST) $ genBST genKey genValue
    k <- gen genKey
    v <- gen genValue
    assert $ P.eq
      .$ ("lhs", toList (insert k v t))
      .$ ("rhs", L.insert (k, v) (toList t))

{-------------------------------------------------------------------------------
  Section 4.6 A Note on Generation
-------------------------------------------------------------------------------}

prop_measureWith :: (Show a, Ord a) => Gen a -> Property ()
prop_measureWith key = do
    t <- genWith (Just . showBST) $ genBST key genValue
    k <- gen key
    collect "present" [k `elem` keys t]
    collect "where" $ if
      | t == nil            -> ["empty"]
      | keys t == [k]       -> ["just k"]
      | all (>= k) (keys t) -> ["at start"]
      | all (<= k) (keys t) -> ["at end"]
      | otherwise           -> ["middle"]

prop_measure :: Property ()
prop_measure = prop_measureWith genKey

genSmallKey :: Gen Int
genSmallKey = Gen.integral $ Range.skewedBy 1 (0, 100)

prop_measure_small :: Property ()
prop_measure_small = prop_measureWith genSmallKey
