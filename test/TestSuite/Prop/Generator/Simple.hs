module TestSuite.Prop.Generator.Simple (tests) where

import Test.Tasty
import Test.Tasty.Falsify

import qualified Test.Falsify.Generator           as Gen
import qualified Test.Falsify.Generator.Auxiliary as Gen
import qualified Test.Falsify.Range               as Range
import Data.Function

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Simple" [
      testProperty "prim" $
        testShrinking (>) . gen $
          Gen.prim
    , testGroup "signedWordN" [
          testProperty (show n) $
            testShrinking ((>=) `on` Gen.forgetSign) . gen $
              Gen.signedWordN (Gen.Precision n)
        | n <- [0 .. 9]
        ]
    , testGroup "signedFraction" [
          testProperty (show n) $
            testShrinking ((>=) `on` Gen.forgetSign) . gen $
              Gen.signedFraction (Gen.Precision n)
        | n <- [0 .. 9]
        ]
    , testGroup "integerFromFraction" [
          testProperty (show n) $
            testShrinking (>=) $
            -- By having 'Gen.integerFromFraction' outside the call to 'gen',
            -- we get to see the value generated by 'Gen.signedFraction' in
            -- case of a counterexample.
            Gen.integerFromFraction (Range.between (0, 100)) <$>
              gen (Gen.signedFraction $ Gen.Precision n)
        | n <- [0 .. 9]
        ]
    , testProperty "integer" $
        testShrinking (>=) $ do
        -- It is important that we do not shrink the precision, because if we
        -- reinterpret the same value with less precision, then the result will
        -- not necessarily be smaller (this is basically the counterexample that
        -- is tested in the @mod@ tests in @Demo.TestShrinking).
        n <- gen $ Gen.withoutShrinking $
               Gen.Precision <$> Gen.integral (Range.between (0, 30))
        gen $ Gen.integerWithPrecision n (Range.between (0, 100))
    , testProperty "integral" $
        testShrinking (>=) . gen $
          Gen.integral $ Range.between (0 :: Word, 100)
    ]

