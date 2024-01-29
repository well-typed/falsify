{-# LANGUAGE OverloadedLists #-}

module Test.Falsify.Reexported.Generator.Text where

import Data.Char (chr, ord)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Falsify.Internal.Generator (Gen)
import Test.Falsify.Internal.Range (Range)
import qualified Test.Falsify.Range as Range
import qualified Test.Falsify.Reexported.Generator.Compound as Gen
import qualified Test.Falsify.Reexported.Generator.Simple as Gen


-- | A valid character for a binary number (@'0'@ or @'1'@)
binit :: Gen Char
binit = Gen.choose (pure '0') (pure '1')


-- | A valid character for an octal number (@'0'@-@'7'@)
octit :: Gen Char
octit = Gen.inRange $ chr <$> Range.between (ord '0', ord '7')


-- | A valid character for a decimal number (@'0'@-@'9'@)
digit :: Gen Char
digit = Gen.inRange $ chr <$> Range.between (ord '0', ord '9')


{- | A valid character for a hexadecimal number (@'0'@-@\'f'@)

 Generates both lower- and uppercase @\'a'@-@\'f'@, but this isn't considered for the distribution
 of characters:
 The change of generating either @\'a'@ or @\'A'@ is the same as generating @'4'@
-}
hexit :: Gen Char
hexit =
    Gen.frequency
        [ (10, digit)
        , (6, Gen.elem $ 'a' :| "AbBcCdDeEfF")
        ]


{-
    0x00 - 0x08: Control characters
    0x09 - 0x0d: Whitespace
    0x0e - 0x1f: Control Characters
    0x20       : Whitespace
    0x21 - 0x23: Punctuation
    0x24       : Symbol
    0x25 - 0x2a: Punctuation
    0x2b       : Symbol
    0x2c - 0x2f: Punctuation
    0x21 - 0x2f: Symbols
    0x30 - 0x39: Digits
    0x3a - 0x3b: Punctuation
    0x3c - 0x3e: Symbols
    0x3f - 0x40: Punctuation
    0x41 - 0x5a: Letters (uppercase)
    0x5b - 0x5d: Punctuation
    0x5e       : Symbol
    0x5f       : Punctuation
    0x60       : Symbol
    0x61 - 0x7a: Letters (lowercase)
    0x7b       : Punctuation
    0x7c       : Symbols
    0x7d       : Punctuation
    0x7e       : Symbols
    0x7f       : Control

-}

-- | A lowercase ascii character (@\'a'@-@\'z'@)
asciiLower :: Gen Char
asciiLower = Gen.inRange $ chr <$> Range.between (ord 'a', ord 'z')


-- | An uppercase ascii character (@\'A'@-@\'Z'@)
asciiUpper :: Gen Char
asciiUpper = Gen.inRange $ chr <$> Range.between (ord 'A', ord 'Z')


-- | An ascii characted, either lowercase or uppercase (@\'a'@-@\'z'@ and @\'A'@-@\'Z'@)
asciiAlpha :: Gen Char
asciiAlpha = Gen.oneof [asciiLower, asciiUpper]


-- | An ascii character or a digit (@\'a'@-@\'z'@, @\'A'@-@\'Z'@ and @'0'@-@'9'@)
asciiAlphaNum :: Gen Char
asciiAlphaNum = Gen.oneof [asciiLower, asciiUpper, digit]

-- | An ascii space character (regular space or one of \t \n \v \f \r)
asciiWhitespace :: Gen Char
asciiWhitespace =
    Gen.elem $
        chr
            <$> [ 0x20 -- the normal space character
                , 0x9 -- \t (horizontal tab)
                , 0xa -- \n (newline)
                , 0xb -- \v (vertical tab)
                , 0xc -- \f (form feed)
                , 0xd -- \r (carriage return)
                ]

-- | An ascii symbol (one of $ + < = > ^ ` | ~)
asciiSymbol :: Gen Char
asciiSymbol =
    Gen.elem $
        chr
            <$> [ 0x24 -- Dollar sign ($) (-- $ causes problems on ghc <9)
                , 0x2b -- +
                , 0x3c -- <
                , 0x3d -- =
                , 0x3e -- >
                , 0x5e -- Caret sign (^) (-- ^ causes problems on ghc <9)
                , 0x60 -- `
                , 0x7c -- Pipe sign (|) (-- | causes problems on ghc <9)
                , 0x7e -- ~
                ]

-- | An ascii punctuation character (one of ! " $ % & ' ( ) * , - . / : ; ? @ [ \\ ] _ { })
asciiPunctuation :: Gen Char
asciiPunctuation =
    Gen.elem $
        chr
            <$> [ 0x21 -- !
                , 0x22 -- "
                , 0x23 -- #
                , 0x25 -- %
                , 0x26 -- &
                , 0x27 -- '
                , 0x28 -- (
                , 0x29 -- )
                , 0x2a -- Star symbol (*) (-- * causes problems on ghc <9)
                , 0x2c -- ,
                , 0x2d -- -
                , 0x2e -- .
                , 0x2f -- /
                , 0x3a -- :
                , 0x3b -- ;
                , 0x3f -- ?
                , 0x40 -- @
                , 0x5b -- [
                , 0x5c -- \
                , 0x5d -- ]
                , 0x5f -- _
                , 0x7b -- {
                , 0x7d -- }
                ]

-- | An ascii control character
asciiControl :: Gen Char
asciiControl =
    Gen.elem $
        chr
            <$> ( [0x00 .. 0x08]
                    <> [0x0e .. 0x1f]
                    <> [0x7f]
                )


-- | An ascii character but not null or a control character.
ascii :: Gen Char
ascii = Gen.oneof [asciiAlphaNum, asciiWhitespace, asciiSymbol, asciiPunctuation]


{- | An ascii character in the full 7-bit range. (@'\0'@-@'\127'@)

 This includes control chars and the null character
-}
asciiAll :: Gen Char
asciiAll = Gen.oneof [ascii, asciiControl]


{- | A Unicode character excluding noncharacters and invalid standalone surrogates:
   @'\0'..'\1114111' (excluding '\55296'..'\57343', '\65534', '\65535')@
-}
unicode :: Gen Char
unicode =
    let
        ranges =
            [ (97, 122) -- lowercase letters
            , (65, 90) -- uppercase letters
            , (48, 57) -- digits
            , (32, 32) -- space
            , (0, 31)
            , (33, 47)
            , (58, 64)
            , (91, 96)
            , (123, 55295)
            , (57344, 65533)
            , (65536, 1114111)
            ]
        frequencies =
            map
                ( \(low, high) ->
                    ( fromIntegral $ high - low + 1
                    , Gen.inRange $ Range.between (low, high)
                    )
                )
                ranges
     in
        chr <$> Gen.frequency frequencies


{- | A Unicode character excluding noncharacters and invalid standalone surrogates:
   'unicode', but doesn't generate charactes with White_Space=yes or "Related Unicode characters"
   as per https://en.wikipedia.org/wiki/Whitespace_character (2023-06-04)
-}
unicodeNonSpace :: Gen Char
unicodeNonSpace =
    let
        ranges =
            [ (97, 122) -- lowercase letters
            , (65, 90) -- uppercase letters
            , (48, 57) -- digits
            , (32, 32) -- space
            , (0, 8)
            , (14, 31)
            , (33, 47)
            , (58, 64)
            , (91, 96)
            , (123, 132)
            , (134, 159)
            , (161, 5759)
            , (5761, 6157)
            , (6159, 8191)
            , (8206, 8231)
            , (8234, 8238)
            , (8240, 8286)
            , (8289, 12287)
            , (12289, 55295)
            , (57344, 65278)
            , (65280, 65533)
            , (65536, ord maxBound)
            ]
        frequencies =
            map
                ( \(low, high) ->
                    ( fromIntegral $ high - low + 1
                    , Gen.inRange $ Range.between (low, high)
                    )
                )
                ranges
     in
        chr <$> Gen.frequency frequencies


-- | Any Unicode character, including noncharacters and invalid standalone surrogates
unicodeAll :: Gen Char
unicodeAll =
    let
        ranges =
            [ (97, 122) -- lowercase letters
            , (65, 90) -- uppercase letters
            , (48, 57) -- digits
            , (32, 32) -- space
            , (0, 31)
            , (33, 47)
            , (58, 64)
            , (91, 96)
            , (123, ord maxBound)
            ]
        frequencies =
            map
                ( \(low, high) ->
                    ( fromIntegral $ high - low + 1
                    , Gen.inRange $ Range.between (low, high)
                    )
                )
                ranges
     in
        chr <$> Gen.frequency frequencies


-- | A string with the given length and composed of characters drawn from the given generator.
string :: Range Word -> Gen Char -> Gen String
string = Gen.list


-- | A text with the given length and composed of characters drawn from the given generator.
text :: Range Word -> Gen Char -> Gen Text
text range gen = Text.pack <$> string range gen
