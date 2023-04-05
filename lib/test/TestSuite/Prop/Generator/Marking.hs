module TestSuite.Prop.Generator.Marking (tests) where

import Control.Monad
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Word
import Test.Tasty
import Test.Tasty.Falsify

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Falsify.Generator (Marked(..), Mark(..))

import qualified Test.Falsify.Generator as Gen hiding (mark)
import qualified Test.Falsify.Predicate as P

import TestSuite.Util.List

tests :: TestTree
tests = testGroup "TestSuite.Prop.Generator.Marking" [
      testGroup "list" [
          testProperty "shrinking" prop_list_shrinking
        , testProperty "minimum"   prop_list_minimum
        ]
    ]

{-------------------------------------------------------------------------------
  Marking
-------------------------------------------------------------------------------}

-- | Mark an element
--
-- Marks as 'Drop' with 50% probability.
--
-- We avoid using 'Gen.mark' here, which depends on @shrinkTo@. This version
-- uses only 'Gen.prim'; the difference in behaviour is that this version of
-- @mark@ can produce elements that are marked as drop from the get-go.
mark :: Gen a -> Gen (Marked Gen a)
mark x = flip Marked x <$> (aux <$> Gen.prim)
  where
    aux :: Word64 -> Mark
    aux n = if n >= maxBound `div` 2 then Keep else Drop

{-------------------------------------------------------------------------------
  List
-------------------------------------------------------------------------------}

genMarkedList :: Gen [(Word, Word64)]
genMarkedList = do
    xs <- forM [0 .. 9] (\i -> mark ((i, ) <$> Gen.prim))
    catMaybes <$> Gen.selectAllKept xs

prop_list_shrinking :: Property ()
prop_list_shrinking =
    testShrinkingOfGen
      (        mconcat [
                          P.flip (P.relatedBy ("isSubsetOf", Set.isSubsetOf))
                   `P.on` P.fn ("keysSet", Map.keysSet)
                 , P.relatedBy ("shrunkCod", shrunkCod)
                 ]
        `P.on` P.transparent Map.fromList
      )
      genMarkedList
  where
    shrunkCod :: Map Word Word64 -> Map Word Word64 -> Bool
    shrunkCod orig shrunk = and [
          -- The 'shrunkDom' check justifies the use of @(!)@ here
          orig Map.! k >= v
        | (k, v) <- Map.toList shrunk
        ]

prop_list_minimum :: Property ()
prop_list_minimum =
    testMinimum (P.satisfies ("expected", expected)) $ do
      xs <- gen $ genMarkedList
      case xs of
        (0, _):_   -> discard
        _otherwise -> return ()
      unless (pairwiseAll (==) $ map snd xs) $ testFailed xs
  where
    expected :: [(Word, Word64)] -> Bool
    expected [(i, 0), (j, 1)] | i < j = True
    expected _otherwise               = False
