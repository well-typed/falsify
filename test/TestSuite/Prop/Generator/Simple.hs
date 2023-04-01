module TestSuite.Prop.Generator.Simple (tests) where

import Control.Monad (unless)
import Data.List (intercalate)
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify.Predicate ((.$))

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range     as Range
import Data.Bits
import Data.Proxy
import Data.Typeable

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Simple" [
    testGroup "prim" [
        testProperty "shrinking" prop_prim_shrinking
      , testGroup "minimum" [
            testProperty (show target) $ prop_prim_minimum target
          | target <- [0 .. 4]
          ]
      ]
  , testGroup "bool" [
        testGroup "towardsFalse" [
            testProperty "shrinking" $ prop_bool_shrinking False
          , testProperty "minimum"   $ prop_bool_minimum   False
          ]
      , testGroup "towardsTrue" [
            testProperty "shrinking" $ prop_bool_shrinking True
          , testProperty "minimum"   $ prop_bool_minimum   True
          ]
      ]
  , testGroup "int" [
        testGroup "between" [
            testGroup (intercalate "_" [show x, show y]) [
                testProperty "shrinking" $ prop_int_between_shrinking (x, y)
              , testGroup "minimum" [
                    testProperty (show target) $
                      prop_int_between_minimum (x, y) target
                  | target <- [0, 1, 99, 100]
                  ]
              ]
          | (x, y) <- [
                (  0,   0)
              , (  0,  10)
              , (  0, 100)
              , ( 10,   0)
              , ( 10,  10)
              , ( 10, 100)
              , (100,   0)
              , (100,  10)
              , (100, 100)
              ]
          ]
      , let test_int_withOrigin :: forall a.
                 (Typeable a, Show a, Integral a, FiniteBits a)
              => Proxy a -> TestTree
            test_int_withOrigin p = testGroup (show $ typeRep p) [
                  testGroup (intercalate "_" [show x, show y, show o]) [
                      testProperty "shrinking" $
                        prop_integral_withOrigin_shrinking @a (x, y) o
                    , testGroup "minimum" [
                          testProperty (show target) $
                            prop_integral_withOrigin_minimum (x, y) o target
                        | target <- [0, 1, 49, 50, 51, 99, 100]
                        ]
                    ]
                | ((x, y), o) <- [
                      ((0,  10),   0)
                    , ((0,  10),  10)
                    , ((0,  10),   5)
                    , ((0, 100),   0)
                    , ((0, 100), 100)
                    , ((0, 100),  50)
                    ]
                ]
        in testGroup "withOrigin" [
               test_int_withOrigin (Proxy @Int)
             , test_int_withOrigin (Proxy @Word)
             ]
      ]
    ]


{-------------------------------------------------------------------------------
  Prim
-------------------------------------------------------------------------------}

-- Gen.prime is the only generator where we a /strict/ inequality
prop_prim_shrinking :: Property ()
prop_prim_shrinking = testShrinkingOfGen P.gt $ Gen.prim

-- The minimum is always 0, unless 0 is not a counter-example
prop_prim_minimum :: Word64 -> Property ()
prop_prim_minimum target = do
    testMinimum (P.eq .$ ("expected", if target == 0 then 1 else 0)) $ do
      x <- gen $ Gen.prim
      unless (x == target) $ testFailed x

{-------------------------------------------------------------------------------
  Bool
-------------------------------------------------------------------------------}

prop_bool_shrinking :: Bool -> Property ()
prop_bool_shrinking False = testShrinkingOfGen P.ge $ Gen.bool False
prop_bool_shrinking True  = testShrinkingOfGen P.le $ Gen.bool True

prop_bool_minimum :: Bool -> Property ()
prop_bool_minimum target =
    testMinimum (P.eq .$ ("target", target)) $ do
      b <- gen $ Gen.bool target
      testFailed b

{-------------------------------------------------------------------------------
  Range: 'between'

  This implicitly tests generation of fractions as well as determining
  precision.
-------------------------------------------------------------------------------}

prop_int_between_shrinking :: (Int, Int) -> Property ()
prop_int_between_shrinking (x, y)
  | x <= y    = testShrinkingOfGen P.ge $ Gen.integral $ Range.between (x, y)
  | otherwise = testShrinkingOfGen P.le $ Gen.integral $ Range.between (x, y)

prop_int_between_minimum :: (Int, Int) -> Int -> Property ()
prop_int_between_minimum (x, y) _target | x == y =
    testMinimum (P.eq .$ ("expected", x)) $ do
      n <- gen $ Gen.integral $ Range.between (x, y)
      -- The only value we can produce here is @x@, so no point looking for
      -- anything these (that would just result in all tests being discarded)
      testFailed n
prop_int_between_minimum (x, y) target =
    testMinimum (P.eq .$ ("expected", expected)) $ do
      n <- gen $ Gen.integral $ Range.between (x, y)
      unless (n == target) $ testFailed n
  where
    expected :: Int
    expected
      | x < y     = if target == x then x + 1 else x
      | otherwise = if target == x then x - 1 else x

{-------------------------------------------------------------------------------
  Range: 'withOrigin'
-------------------------------------------------------------------------------}

prop_integral_withOrigin_shrinking ::
     (Show a, Integral a, FiniteBits a)
  => (a, a) -> a -> Property ()
prop_integral_withOrigin_shrinking (x, y) o =
    testShrinkingOfGen (P.towards o) $
      Gen.integral $ Range.withOrigin (x, y) o

prop_integral_withOrigin_minimum :: forall a.
     (Show a, Integral a, FiniteBits a)
  => (a, a) -> a -> a -> Property ()
prop_integral_withOrigin_minimum (x, y) o _target | x == y =
    testMinimum (P.eq .$ ("expected", x)) $ do
      -- See discussion in 'prop_int_between_minimum'
      n <- gen $ Gen.integral $ Range.withOrigin (x, y) o
      testFailed n
prop_integral_withOrigin_minimum (x, y) o target =
    testMinimum (P.elem .$ ("expected", expected)) $ do
      n <- gen $ Gen.integral $ Range.withOrigin (x, y) o
      unless (n == target) $ testFailed n
  where
    expected :: [a]
    expected
      | target == o = [o + 1, o - 1]
      | otherwise   = [o]
