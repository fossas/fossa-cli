module GraphingSpec (
  spec,
) where

import GraphUtil
import Graphing
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "unfold" $ do
    it "should unfold deeply" $ do
      let graph :: Graphing Int
          graph = unfold [10] (\x -> if x > 0 then [x -2] else []) id

      expectDirect [10] graph
      expectDeps [10, 8, 6, 4, 2, 0] graph
      expectEdges [(10, 8), (8, 6), (6, 4), (4, 2), (2, 0)] graph

  describe "deep" $ do
    it "should add deep node to graphing" $ do
      let graph :: Graphing Int
          graph = Graphing.deep 5 <> Graphing.edge 2 3
      expectDirect [] graph
      expectDeps [5, 2, 3] graph
      expectEdges [(2, 3)] graph

  describe "promoteToDirect" $ do
    it "should promote nodes to direct" $ do
      let graph :: Graphing Int
          graph = Graphing.promoteToDirect (< 5) (unfold [10] (\x -> if x > 0 then [x -2] else []) id)
      expectDirect [0, 2, 4, 10] graph
      expectDeps [10, 8, 6, 4, 2, 0] graph
      expectEdges [(10, 8), (8, 6), (6, 4), (4, 2), (2, 0)] graph

  describe "shrinkSingle" $ do
    it "should preserve root node relationships" $ do
      -- 1 -> 2 -> 3 -> 4 with 1 and 2 as direct
      --
      -- -> shrinkSingle 2
      --
      -- 1 -> 3 -> 4 with 1 and 3 as direct
      let graph :: Graphing Int
          graph = Graphing.directs [1, 2] <> Graphing.edges (zip [1 .. 3] [2 .. 4])

          graph' :: Graphing Int
          graph' = Graphing.shrinkSingle 2 graph

      expectDirect [1, 3] graph'
      expectDeps [1, 3, 4] graph'
      expectEdges [(1, 3), (3, 4)] graph'

    it "should preserve multiple outgoing edges" $ do
      -- 1 -> 2 -> 3 --> 4
      --             \-> 5
      --
      -- -> shrinkSingle 3
      --
      -- 1 -> 2 --> 4
      --        \-> 5
      let graph :: Graphing Int
          graph = Graphing.edges [(1, 2), (2, 3), (3, 4), (3, 5)]

          graph' :: Graphing Int
          graph' = Graphing.shrinkSingle 3 graph

      expectDirect [] graph'
      expectDeps [1, 2, 4, 5] graph'
      expectEdges [(1, 2), (2, 4), (2, 5)] graph'

  describe "shrink" $ do
    it "should collapse edges through several nodes" $ do
      let graph :: Graphing Int
          graph = Graphing.edges (zip [1 .. 4] [2 .. 5])

          graph' :: Graphing Int
          graph' = Graphing.shrink (\x -> x /= 2 && x /= 3) graph

      expectDirect [] graph'
      expectDeps [1, 4, 5] graph'
      expectEdges [(1, 4), (4, 5)] graph'
