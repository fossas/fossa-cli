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

expectDeps :: [G.Dependency] -> G.Graph -> Expectation
expectDeps deps graph = toList (G.graphDeps graph) `shouldMatchList` deps

expectEdges :: [(G.Dependency, G.Dependency)] -> G.Graph -> Expectation
expectEdges edges graph = traverse_ expectEdge edges *> (length assocs `shouldBe` length edges)
  where
  expectEdge (dep1, dep2) = flip shouldSatisfy (not . null) $
    [ () | (parentRef, children) <- IM.toList assocs
         , S.index deps parentRef == dep1
         , childRef              <- IS.toList children
         , S.index deps childRef  == dep2
         ]
  deps   = G.graphDeps graph
  assocs = G.graphAssocs graph

expectDirect :: [G.Dependency] -> G.Graph -> Expectation
expectDirect expected graph = map (S.index deps) (IS.toList direct) `shouldMatchList` expected
  where
  deps   = G.graphDeps graph
  direct = G.graphDirect graph
