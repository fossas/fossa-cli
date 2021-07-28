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
          graph = Graphing.deep 5 $ Graphing.edge 2 3 (Graphing.empty)
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
