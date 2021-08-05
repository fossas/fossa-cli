module GraphUtil (
  expectDeps,
  expectDep,
  expectDirect,
  expectEdges,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Data.Foldable (traverse_)
import Graphing (Graphing)
import Graphing qualified
import Test.Hspec (
  Expectation,
  shouldBe,
  shouldContain,
  shouldMatchList,
  shouldSatisfy,
 )

-- TODO: expectReachable instead?

-- | Expect the given dependencies to be the deps in the graph
expectDeps :: (Ord a, Show a) => [a] -> Graphing a -> Expectation
expectDeps deps graph = Graphing.vertexList graph `shouldMatchList` deps

-- | Expects the given `a` to exist in the `Graphing`
expectDep :: (Ord a, Show a) => a -> Graphing a -> Expectation
expectDep dep graph = Graphing.vertexList graph `shouldContain` [dep]

-- TODO: I expect the shouldSatisfy will produce poor test failure messages

-- | Expect only the given edges between @[(parent,child)]@ dependencies to be present in the graph
expectEdges :: (Ord a, Show a) => [(a, a)] -> Graphing a -> Expectation
expectEdges edges graph =
  (length edges `shouldBe` AM.edgeCount (Graphing.toAdjacencyMap graph))
    *> traverse_ (`shouldSatisfy` \(from, to) -> AM.hasEdge from to (Graphing.toAdjacencyMap graph)) edges

-- | Expect the given dependencies to be the direct deps in the graph
expectDirect :: (Ord a, Show a) => [a] -> Graphing a -> Expectation
expectDirect expected graph = Graphing.directList graph `shouldMatchList` expected
