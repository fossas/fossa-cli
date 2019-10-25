module GraphUtil
  ( expectDeps
  , expectDirect
  , expectEdges
  ) where

import Prologue
import Test.Tasty.Hspec

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as S

import qualified Graph as G

-- | Expect the given dependencies to be the deps in the graph
expectDeps :: [G.Dependency] -> G.Graph -> Expectation
expectDeps deps graph = toList (G.graphDeps graph) `shouldMatchList` deps

-- | Expect only the given edges between @[(parent,child)]@ dependencies to be present in the graph
expectEdges :: [(G.Dependency, G.Dependency)] -> G.Graph -> Expectation
expectEdges edges graph = traverse_ expectEdge edges *> (length (concatMap IS.toList assocs) `shouldBe` length edges)
  where
  expectEdge (dep1, dep2) = flip shouldSatisfy (not . null) $
    [ () | (parentRef, children) <- IM.toList assocs
         , S.index deps parentRef == dep1
         , childRef              <- IS.toList children
         , S.index deps childRef  == dep2
         ]
  deps   = G.graphDeps graph
  assocs = G.graphAssocs graph

-- | Expect the given dependencies to be the direct deps in the graph
expectDirect :: [G.Dependency] -> G.Graph -> Expectation
expectDirect expected graph = map (S.index deps) (IS.toList direct) `shouldMatchList` expected
  where
  deps   = G.graphDeps graph
  direct = G.graphDirect graph
