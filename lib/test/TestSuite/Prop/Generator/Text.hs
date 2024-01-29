module TestSuite.Prop.Generator.Text (tests) where

import Control.Monad
import Data.Default
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import qualified Data.Set as Set

import Test.Falsify.Generator (Fun)

import Data.Char (GeneralCategory (NotAssigned), digitToInt, generalCategory, isAlpha, isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isControl, isPrint, isPunctuation, isSpace, isSymbol, ord)
import Data.Functor.Contravariant ((>$<))
import Debug.Trace (trace)
import GHC.Char (chr)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as Range
import Test.QuickCheck.Property (Prop)
import Test.Tasty.Runners (TestTree (TestGroup))


tests :: TestTree
tests =
    testGroup
        "TestSuite.Prop.Generator.Text"
        [ testGroup
            "Chars"
            [ testGroup
                "binit"
                [ shrinksDownNumerically Gen.binit
                , shrinksTowards Gen.binit '0'
                , testProperty "is 0 or 1" $ prop_numericallyBetween Gen.binit 0 1
                ]
            , testGroup
                "octit"
                [ shrinksDownNumerically Gen.octit
                , shrinksTowards Gen.octit '0'
                , testProperty "is 0-7" $ prop_numericallyBetween Gen.octit 0 7
                ]
            , testGroup
                "digit"
                [ shrinksDownNumerically Gen.digit
                , shrinksTowards Gen.digit '0'
                , testProperty "isAsciiLower" $ prop_numericallyBetween Gen.digit 0 9
                ]
            , testGroup
                "hexit"
                [ shrinksDownNumerically Gen.hexit
                , shrinksTowards Gen.hexit '0'
                , testProperty "isAsciiLower" $ prop_numericallyBetween Gen.hexit 0 0xf
                ]
            , testGroup
                "asciiLower"
                [ shrinksDown Gen.asciiLower
                , shrinksTowards Gen.asciiLower 'a'
                , testProperty "isAsciiLower" $ prop_domain Gen.asciiLower isAsciiLower
                ]
            , testGroup
                "asciiUpper"
                [ shrinksDown Gen.asciiUpper
                , shrinksTowards Gen.asciiUpper 'A'
                , testProperty "isAsciiUpper" $ prop_domain Gen.asciiUpper isAsciiUpper
                ]
            , testGroup
                "asciiAlpha"
                [ shrinksTowards Gen.asciiAlpha 'a'
                , testProperty "isAscii and isAlpha" $
                    prop_domain Gen.asciiAlpha (\c -> isAscii c && isAlpha c)
                ]
            , testGroup
                "asciiAlphaNum"
                [ shrinksTowards Gen.asciiAlphaNum 'a'
                , testProperty "in a-zA-Z0-9" $
                    prop_domain Gen.asciiAlphaNum (\c -> isAscii c && isAlphaNum c)
                ]
            , testGroup
                "asciiWhitespace"
                [ shrinksTowards Gen.asciiWhitespace ' '
                , testProperty "is ascii and space" $
                    prop_domain Gen.asciiWhitespace (\c -> isAscii c && isSpace c)
                ]
            , testGroup
                "asciiSymbol"
                [ shrinksTowards Gen.asciiSymbol '$'
                , testProperty "is ascii and symbol" $
                    prop_domain Gen.asciiSymbol (\c -> isAscii c && isSymbol c)
                ]
            , testGroup
                "asciiPunctuation"
                [ shrinksTowards Gen.asciiPunctuation '!'
                , testProperty "is ascii and punctuation" $
                    prop_domain Gen.asciiPunctuation (\c -> isAscii c && isPunctuation c)
                ]
            , testGroup
                "asciiControl"
                [ shrinksTowards Gen.asciiControl '\NUL'
                , testProperty "is ascii and control" $
                    prop_domain Gen.asciiControl (\c -> isAscii c && isControl c)
                ]
            , testGroup
                "ascii"
                [ shrinksTowards Gen.ascii 'a'
                , testProperty "is ascii and (printable or whitespace)" $
                    prop_domain Gen.ascii (\c -> isAscii c && (isPrint c || isSpace c))
                ]
            , testGroup
                "asciiAll"
                [ shrinksTowards Gen.asciiAll 'a'
                , testProperty "is ascii" $
                    prop_domain Gen.asciiAll isAscii
                ]
            , testGroup
                "unicode"
                [ shrinksTowards Gen.unicode 'a'
                , testProperty "is not a noncharacter or invalid standalone surrogate" $
                    prop_domain
                        Gen.unicode
                        ( \c ->
                            not $
                                any
                                    ($ ord c)
                                    [ \x -> 55296 <= x && x <= 57343
                                    , (== 65534)
                                    , (== 65535)
                                    ]
                        )
                ]
            , testGroup
                "unicodeNonSpace"
                [ shrinksTowards Gen.unicodeNonSpace 'a'
                , testProperty "is not space" $ prop_domain Gen.unicodeNonSpace (not . isSpace)
                ]
            , testGroup
                "unicodeAll"
                [ shrinksTowards Gen.unicodeAll 'a'
                ]
            ]
        , testGroup
            "string"
            [ -- TODO
            ]
        , testGroup
            "text"
            [ -- TODO
            ]
        ]


-- -- These tests are pretty slow
-- fewerTests :: TestOptions
-- fewerTests = def {
--     overrideNumTests = Just 10
-- }

{-------------------------------------------------------------------------------
  Single-char generators
-------------------------------------------------------------------------------}

shrinksDownNumerically :: Gen Char -> TestTree
shrinksDownNumerically =
    testProperty "shrinks down (after digitToInt)" . testShrinkingOfGen digitGE
  where
    digitGE = P.relatedBy ("numerically greated", \a b -> digitToInt a >= digitToInt b)


shrinksDown :: Gen Char -> TestTree
shrinksDown =
    testProperty "shrinks down (after digitToInt)" . testShrinkingOfGen P.ge


shrinksTowards :: (Show a, Eq a) => Gen a -> a -> TestTree
shrinksTowards generator expectedMinimum =
    testProperty ("Shrinks towards " <> show expectedMinimum) $
        prop_shrinksTowards generator expectedMinimum


prop_shrinksTowards :: (Show a, Eq a) => Gen a -> a -> Property ()
prop_shrinksTowards generator expectedMinimum =
    testMinimum (P.expect expectedMinimum) $ do
        value <- gen generator
        testFailed value


testDigitRange :: Int -> Int -> Gen Char -> Property ()
testDigitRange lower upper generator =
    testGen (P.between lower upper) $ digitToInt <$> generator


-- testDomainRanges :: (Ord a, Show a) => Gen a -> [(a, a)] -> Property ()
-- testDomainRanges generator ranges =
--     testGen (P.satisfies ("c", \c -> any (\(low, high) -> low <= c && c <= high) ranges)) generator

prop_domain :: (Show a) => Gen a -> (a -> Bool) -> Property ()
prop_domain generator predicate = testGen (P.satisfies ("c", predicate)) generator


prop_numericallyBetween :: Gen Char -> Int -> Int -> Property ()
prop_numericallyBetween generator low high =
    prop_domain generator (\c -> let x = digitToInt c in low <= x && x <= high)
