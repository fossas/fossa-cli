module AdjacencyMapSpec (spec) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Extra qualified as AME
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

-- 1 -> 2
-- 3 -> 4
beforeTrivial :: AM.AdjacencyMap Int
beforeTrivial = AM.edges [(1, 2), (3, 4)]

afterTrivial :: Set (AM.AdjacencyMap Int)
afterTrivial = Set.fromList [AM.edge 1 2, AM.edge 3 4]

-- 1 -> 2
--       \
--         -> 5
--       /
-- 3 -> 4
beforeWithMutual :: AM.AdjacencyMap Int
beforeWithMutual =
  AM.edges
    [ (1, 2)
    , (3, 4)
    , (2, 5)
    , (4, 5)
    ]

-- 1 -> 2 -> 5  (Graph 1)
-- 3 -> 4 -> 5  (Graph 2)
afterWithMutual :: Set (AM.AdjacencyMap Int)
afterWithMutual =
  Set.fromList
    [ AM.edges [(1, 2), (2, 5)]
    , AM.edges [(3, 4), (4, 5)]
    ]

cyclic :: AM.AdjacencyMap Int
cyclic = AM.circuit [1, 2, 3]

checkResult :: (Ord a, Show a) => AM.AdjacencyMap a -> Set (AM.AdjacencyMap a) -> Expectation
checkResult before after = (Set.fromList <$> AME.splitGraph before) `shouldBe` (Just after)

spec :: Spec
spec =
  describe "splitGraph" $ do
    it "should correctly split trivial acyclic graphs" $
      checkResult beforeTrivial afterTrivial

    it "should correctly split a graph with mutual children" $
      checkResult beforeWithMutual afterWithMutual

    it "should reject cyclic graphs" $
      AME.splitGraph cyclic `shouldBe` Nothing
