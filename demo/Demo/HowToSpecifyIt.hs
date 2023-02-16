-- | Examples from "How to Specify It!: A Guide to Writing Properties of Pure
-- Functions", John Hughes, 2020, LNCS 12053.
module Demo.HowToSpecifyIt (tests) where

import Data.Default
import Test.Tasty
import Test.Tasty.Falsify

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
    ]

{-------------------------------------------------------------------------------
  Section 2: "A Primer in Property-Based Testing"
-------------------------------------------------------------------------------}

forAllLists :: ([Int] -> Bool) -> Property ()
forAllLists p = do
    xs <- gen $ Gen.list (Range.between (0, 100)) $
            Gen.enum (Range.between (0, 100))
    assert (show xs) $ p xs

prop_reverse_reverse :: Property ()
prop_reverse_reverse = forAllLists $ \xs -> reverse (reverse xs) == xs

prop_reverse_id :: Property ()
prop_reverse_id = forAllLists $ \xs -> reverse xs == xs
