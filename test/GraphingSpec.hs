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
          graph = unfold [10] (\x -> if x > 0 then [x - 2] else []) id

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
          graph = Graphing.promoteToDirect (< 5) (unfold [10] (\x -> if x > 0 then [x - 2] else []) id)
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

  describe "shrinkWithoutPromotionToDirect" $ do
    it "should preserve set of direct nodes" $ do
      -- 1 -> 2 -> 5 -> 6
      --      \    \
      --       \    7
      -- 3 ----> 4

      let graph :: Graphing Int
          graph = Graphing.edges [(1, 2), (3, 4), (2, 4), (2, 5), (5, 7), (5, 6)] <> Graphing.directs [1, 3]

          graph' :: Graphing Int
          graph' = Graphing.shrinkWithoutPromotionToDirect (\x -> x /= 3 && x /= 5) graph

      expectDirect [1] graph'
      expectDeps [1, 2, 4, 6, 7] graph'
      expectEdges [(1, 2), (2, 4), (2, 6), (2, 7)] graph'

    it "should promote to direct, if and only if no predecessors exist" $ do
      --   1 -> 2 -> 5 -> 6
      --        \       \
      --         \       7
      --   3 ----> 4
      --    \
      --     8

      let graph :: Graphing Int
          graph = Graphing.edges [(1, 2), (3, 4), (3, 8), (2, 4), (2, 5), (5, 7), (5, 6)] <> Graphing.directs [1, 3]

          graph' :: Graphing Int
          graph' = Graphing.shrinkWithoutPromotionToDirect (\x -> x /= 3 && x /= 5) graph

      expectDirect [1, 8] graph'
      expectDeps [1, 2, 4, 6, 7, 8] graph'
      expectEdges [(1, 2), (2, 4), (2, 6), (2, 7)] graph'

  describe "stripRoot" $ do
    let graph :: Graphing Int
        graph = Graphing.directs [1] <> Graphing.edges [(1, 2), (1, 3), (2, 4), (3, 6)]

    it "should promote immediate children as direct nodes" $ do
      let graph' = Graphing.stripRoot graph
      expectDirect [2, 3] graph'

    it "should preserve current root nodes in the graphing as nodes" $ do
      let graph' = Graphing.stripRoot graph
      expectDeps [1, 2, 3, 4, 6] graph'

    it "should preserve edges of current root nodes in the graphing" $ do
      let graph' = Graphing.stripRoot graph
      expectEdges [(1, 2), (1, 3), (2, 4), (3, 6)] graph'

  describe "shrinkRoots" $ do
    let graph :: Graphing Int
        graph = Graphing.directs [1] <> Graphing.edges [(1, 2), (1, 3), (2, 4), (3, 6)]

    it "should remove direct nodes" $ do
      let graph' = Graphing.shrinkRoots graph

      expectDirect [2, 3] graph'
      expectDeps [2, 3, 4, 6] graph'
      expectEdges [(2, 4), (3, 6)] graph'

    it "should not modify when there are no direct nodes" $ do
      let graphWithoutDirectNodes :: Graphing Int
          graphWithoutDirectNodes = Graphing.edges [(1, 2), (1, 3), (2, 4), (3, 6)]

          graph' :: Graphing Int
          graph' = Graphing.shrinkRoots graphWithoutDirectNodes

      expectDirect [] graph'
      expectDeps [1, 2, 3, 4, 6] graph'
      expectEdges [(1, 2), (1, 3), (2, 4), (3, 6)] graph'

  describe "hasPredecessors" $ do
    it "should report False when node has no predecessors" $ do
      --  1 -> 2 -> 3 -> 4
      let graph :: Graphing Int = Graphing.directs [1] <> Graphing.edges [(1, 2), (2, 3), (3, 4)]
      hasPredecessors graph 1 `shouldBe` False

    it "should report True when node has predecessors" $ do
      --  1 -> 2 -> 3 -> 4
      let graph :: Graphing Int = Graphing.directs [1, 2] <> Graphing.edges [(1, 2), (2, 3), (3, 4)]
      hasPredecessors graph 2 `shouldBe` True

  describe "getRootsOf" $ do
    it "should report direct node origins" $ do
      --   1 -> 2
      --    \    \
      --     3    \
      --      \    4
      --       6
      let graph :: Graphing Int = Graphing.directs [1] <> Graphing.edges [(1, 2), (1, 3), (2, 4), (3, 6)]
      getRootsOf graph 4 `shouldBe` [1]

    it "should report multiple direct node origins" $ do
      --   1 -> 2
      --    \    \
      --     3    \
      --      \    4
      --       6
      let graph :: Graphing Int = Graphing.directs [1, 2] <> Graphing.edges [(1, 2), (1, 3), (2, 4), (3, 6)]
      getRootsOf graph 4 `shouldContain` [1]
      getRootsOf graph 4 `shouldContain` [2]

    it "should report direct node origins even when graphing is cyclic" $ do
      --   1 -> 2 -> 3 -> 4 -> 5
      --         ↑	     /
      --          -------
      let graph :: Graphing Int = Graphing.directs [1] <> Graphing.edges [(1, 2), (2, 3), (3, 4), (4, 2), (4, 5)]
      getRootsOf graph 4 `shouldBe` [1]

    it "should not report itself when queried node is direct node" $ do
      --  1 -> 2 -> 3 -> 4
      let graph :: Graphing Int = Graphing.directs [1] <> Graphing.edges [(1, 2), (2, 3), (3, 4)]
      getRootsOf graph 1 `shouldBe` []
