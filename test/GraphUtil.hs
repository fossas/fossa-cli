module GraphUtil (
  expectDeps,
  expectDeps',
  expectDep,
  expectDep',
  expectEdge,
  expectEdge',
  expectEdges,
  expectEdges',
  expectDirect,
  expectDirect',
  expectGraphEqual,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Effect.Lift (Has, Lift, sendIO)
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

-- | Effectful version of 'expectDeps'
expectDeps' :: (Ord a, Show a, Has (Lift IO) sig m) => [a] -> Graphing a -> m ()
expectDeps' deps graph = sendIO $ expectDeps deps graph

-- | Expects the given `a` to exist in the `Graphing`
expectDep :: (Ord a, Show a) => a -> Graphing a -> Expectation
expectDep dep graph = Graphing.vertexList graph `shouldContain` [dep]

-- | Effectful version of 'expectDep'
expectDep' :: (Ord a, Show a, Has (Lift IO) sig m) => a -> Graphing a -> m ()
expectDep' dep graph = sendIO $ expectDep dep graph

-- TODO: I expect the shouldSatisfy will produce poor test failure messages

-- | Expect only the given edges between @[(parent,child)]@ dependencies to be present in the graph
expectEdges :: (Ord a, Show a) => [(a, a)] -> Graphing a -> Expectation
expectEdges edges graph =
  (AM.edgeCount (Graphing.toAdjacencyMap graph) `shouldBe` length edges)
    *> traverse_ (`shouldSatisfy` \(from, to) -> AM.hasEdge from to (Graphing.toAdjacencyMap graph)) edges

-- This assertion is meant to give more detailed information when two graphs are not equal to each other.
expectGraphEqual :: (Ord a, Show a) => Graphing a -> Graphing a -> Expectation
expectGraphEqual g1 g2 = do
  Graphing.vertexList g1 `shouldMatchList` Graphing.vertexList g2
  Graphing.edgesList g1 `shouldMatchList` Graphing.edgesList g2

-- | Effectful version of 'expectEdges'
expectEdges' :: (Ord a, Show a, Has (Lift IO) sig m) => [(a, a)] -> Graphing a -> m ()
expectEdges' edges graph = sendIO $ expectEdges edges graph

-- | Expect the given dependencies to be the direct deps in the graph
expectDirect :: (Ord a, Show a) => [a] -> Graphing a -> Expectation
expectDirect expected graph = Graphing.directList graph `shouldMatchList` expected

expectDirect' :: (Ord a, Show a, Has (Lift IO) sig m) => [a] -> Graphing a -> m ()
expectDirect' expected graph = sendIO $ expectDirect expected graph

expectEdge :: (Ord a, Show a) => Graphing a -> a -> a -> Expectation
expectEdge graph expectedFrom expectedTo = Graphing.edgesList graph `shouldContain` [(expectedFrom, expectedTo)]

expectEdge' :: (Ord a, Show a, Has (Lift IO) sig m) => Graphing a -> a -> a -> m ()
expectEdge' graph expectedFrom expectedTo = sendIO $ expectEdge graph expectedFrom expectedTo
