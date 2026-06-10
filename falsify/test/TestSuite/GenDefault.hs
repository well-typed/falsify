{-# LANGUAGE UndecidableInstances #-}

-- | Test the 'GenDefault' machinery
--
-- We define a tag, derive some 'GenDefault' instances, and asserting that the
-- derived generators yield more than one distinct value.
module TestSuite.GenDefault (tests) where

import Control.Monad
import Data.Proxy
import GHC.Exts (IsList, IsString)
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as Set

import Test.Falsify
import Test.Falsify.GenDefault hiding (GenDefault)
import Test.Falsify.Interactive (sample)

{-------------------------------------------------------------------------------
  Example tag
-------------------------------------------------------------------------------}

data Tag

{-------------------------------------------------------------------------------
  ViaTag
-------------------------------------------------------------------------------}

deriving via ViaTag Std Int  instance GenDefault Tag Int
deriving via ViaTag Std Char instance GenDefault Tag Char

{-------------------------------------------------------------------------------
  ViaList
-------------------------------------------------------------------------------}

newtype AList a = AList [a]
  deriving newtype (Eq, Ord, Show, IsList)

deriving
  via ViaList (AList a) 0 2
  instance GenDefault Tag a => GenDefault Tag (AList a)

{-------------------------------------------------------------------------------
  ViaString
-------------------------------------------------------------------------------}

newtype AString = AString String
  deriving newtype (Eq, Ord, Show, IsString)
  deriving (GenDefault Tag) via (ViaString AString 0 2)

{-------------------------------------------------------------------------------
  ViaEnum
-------------------------------------------------------------------------------}

data Choice = ChoiceA | ChoiceB
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (GenDefault Tag) via (ViaEnum Choice)

{-------------------------------------------------------------------------------
  ViaGeneric
-------------------------------------------------------------------------------}

deriving
  via ViaGeneric Tag (Maybe a)
  instance GenDefault Tag a => GenDefault Tag (Maybe a)

data Record = Record !Int !(Maybe Record)
  deriving stock (Eq, Ord, Show, Generic)
  deriving (GenDefault Tag) via (ViaGeneric Tag Record)

{-------------------------------------------------------------------------------
  Sanity check: verify that the generators can produce more than one value
-------------------------------------------------------------------------------}

data GenCase where
  GenCase :: Ord a => String -> Gen a -> GenCase

genDefaultByProxy :: GenDefault Tag a => Proxy a -> Gen a
genDefaultByProxy _ = genDefault (Proxy @Tag)

mkGenCase :: (Ord a, GenDefault Tag a) => String -> Proxy a -> GenCase
mkGenCase name = GenCase name . genDefaultByProxy

genCases :: [GenCase]
genCases = [
      mkGenCase "Int"     (Proxy @Int)
    , mkGenCase "Char"    (Proxy @Char)
    , mkGenCase "Choice"  (Proxy @Choice)
    , mkGenCase "AList"   (Proxy @(AList Char))
    , mkGenCase "AString" (Proxy @AString)
    , mkGenCase "Record"  (Proxy @Record)
    ]

testGenCase :: GenCase -> TestTree
testGenCase (GenCase name g) = testCase name $ do
    xs <- fmap Set.fromList (replicateM 20 (sample g))
    assertBool "generates more than one value" (Set.size xs > 1)

tests :: TestTree
tests = testGroup "TestSuite.GenDefault" (fmap testGenCase genCases)
