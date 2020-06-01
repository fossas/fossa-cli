module GraphUtil
  ( expectDeps
  , expectDirect
  , expectEdges
  ) where

import Prologue
import Test.Hspec

import qualified Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.ToGraph (vertexSet)

import Graphing

-- TODO; expectReachable instead?
-- | Expect the given dependencies to be the deps in the graph
expectDeps :: (Ord a, Show a) => [a] -> Graphing a -> Expectation
expectDeps deps graph = toList (vertexSet (graphingAdjacent graph)) `shouldMatchList` deps

-- TODO: I expect the shouldSatisfy will produce poor test failure messages
-- | Expect only the given edges between @[(parent,child)]@ dependencies to be present in the graph
expectEdges :: (Ord a, Show a) => [(a,a)] -> Graphing a -> Expectation
expectEdges edges graph = (length edges `shouldBe` AM.edgeCount (graphingAdjacent graph)) *> traverse_ (`shouldSatisfy` \(from,to) -> AM.hasEdge from to (graphingAdjacent graph)) edges

-- | Expect the given dependencies to be the direct deps in the graph
expectDirect :: (Eq a, Show a) => [a] -> Graphing a -> Expectation
expectDirect expected graph = toList (graphingDirect graph) `shouldMatchList` expected
