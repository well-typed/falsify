{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | We test the 'GenDefault' machinery by defining a tag, deriving some 'GenDefault'
-- instances, and asserting that the derived generators yield more than one distinct
-- value.
module TestSuite.GenDefault (tests) where

import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import GHC.Exts (IsList, IsString)
import GHC.Generics (Generic)
import qualified Test.Falsify.GenDefault as FD
import qualified Test.Falsify.GenDefault.Std as FDS
import qualified Test.Falsify.Generator as FG
import qualified Test.Falsify.Interactive as FI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Control.Monad (replicateM)

data Tag

-- Exercise ViaTag

deriving via (FD.ViaTag FDS.Std Int) instance FD.GenDefault Tag Int
deriving via (FD.ViaTag FDS.Std Char) instance FD.GenDefault Tag Char

-- Exercise ViaList

newtype AList a = AList [a]
  deriving newtype (Eq, Ord, Show, IsList)

deriving via (FD.ViaList (AList a) 0 2) instance FD.GenDefault Tag a => FD.GenDefault Tag (AList a)

-- Exercise ViaString

newtype AString = AString String
  deriving newtype (Eq, Ord, Show, IsString)
  deriving (FD.GenDefault Tag) via (FD.ViaString AString 0 2)

-- Exercise ViaEnum

data Choice = ChoiceA | ChoiceB
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (FD.GenDefault Tag) via (FD.ViaEnum Choice)

-- Exercise ViaGeneric

deriving via (FD.ViaGeneric Tag (Maybe a)) instance FD.GenDefault Tag a => FD.GenDefault Tag (Maybe a)

data Record = Record !Int !(Maybe Record)
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FD.GenDefault Tag) via (FD.ViaGeneric Tag Record)

data GenCase where
  GenCase :: Ord a => String -> FG.Gen a -> GenCase

genDefaultByProxy :: FD.GenDefault Tag a => Proxy a -> FG.Gen a
genDefaultByProxy _ = FD.genDefault (Proxy @Tag)

mkGenCase :: (Ord a, FD.GenDefault Tag a) => String -> Proxy a -> GenCase
mkGenCase name = GenCase name . genDefaultByProxy

genCases :: [GenCase]
genCases =
  [ mkGenCase "Int" (Proxy @Int)
  , mkGenCase "Char" (Proxy @Char)
  , mkGenCase "Choice" (Proxy @Choice)
  , mkGenCase "AList" (Proxy @(AList Char))
  , mkGenCase "AString" (Proxy @AString)
  , mkGenCase "Record" (Proxy @Record)
  ]

testGenCase :: GenCase -> TestTree
testGenCase (GenCase name gen) = testCase name $ do
  xs <- fmap Set.fromList (replicateM 10 (FI.sample gen))
  assertBool "generates more than one value" (Set.size xs > 1)

tests :: TestTree
tests = testGroup "TestSuite.GenDefault" (fmap testGenCase genCases)
