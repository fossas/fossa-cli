module GraphingSpec (
  spec,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Extra (splitGraph)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Graphing (
  Graphing,
  color,
  deep,
  deeps,
  direct,
  directList,
  directs,
  edge,
  edges,
  edgesList,
  getRootsOf,
  hasPredecessors,
  promoteToDirect,
  pruneUnreachable,
  reachableSuccessorsWithCondition,
  shrink,
  shrinkRoots,
  shrinkSingle,
  shrinkWithoutPromotionToDirect,
  stripRoot,
  subGraphOf,
  toAdjacencyMap,
  unfold,
  unfoldDeep,
  vertexList,
 )
import Test.Hspec (
  Spec,
  context,
  describe,
  it,
  shouldBe,
  shouldContain,
  shouldMatchList,
 )
import Prelude

unfoldSpec :: Spec
unfoldSpec = do
  describe "unfold" $
    it "Graph children of seed nodes as deep" $ do
      let graph :: Graphing Int
          graph = unfold [10] (\x -> if x > 0 then [x - 2] else []) id

      expectDirect [10] graph
      expectDeps [10, 8, 6, 4, 2, 0] graph
      expectEdges [(10, 8), (8, 6), (6, 4), (4, 2), (2, 0)] graph

  describe "unfoldDeep" $
    it "Graph all nodes, including seeds, as deep" $ do
      let graph :: Graphing Int
          graph = unfoldDeep [10] (\x -> if x > 0 then [x - 2] else []) id

      expectDirect [] graph
      expectDeps [10, 8, 6, 4, 2, 0] graph
      expectEdges [(10, 8), (8, 6), (6, 4), (4, 2), (2, 0)] graph

accessingElementsSpec :: Spec
accessingElementsSpec = describe "Accessing graph elements" $ do
  describe "Listing vertices and edges" $
    do
      let directNodes = [1, 2, 3] :: [Int]
          graphEdges =
            [ (1, 4)
            , (2, 3)
            , (3, 3)
            , (4, 5)
            ]

          graph :: Graphing Int
          graph =
            Graphing.directs directNodes
              <> Graphing.edges graphEdges

      it "directList should list direct nodes" $ do
        Graphing.directList graph `shouldMatchList` [1, 2, 3]

      it "vertexList should list all vertices besides Root" $ do
        Graphing.vertexList graph `shouldMatchList` [1, 2, 3, 4, 5]

      it "edgesList should list all edges" $ do
        Graphing.edgesList graph `shouldMatchList` graphEdges

  describe "toAdjacenyMap" $ do
    it "Does not include the root element" $ do
      let graph :: Graphing Int
          graph = Graphing.directs [1, 2, 3]
          am = AM.vertices [1, 2, 3]
      toAdjacencyMap graph `shouldBe` am

addingNodesSpec :: Spec
addingNodesSpec = context "adding nodes to a Graphing" $ do
  describe "deep" $ do
    it "should add deep node to graphing" $ do
      let graph :: Graphing Int
          graph = Graphing.deep 5 <> Graphing.edge 2 3
      expectDirect [] graph
      expectDeps [5, 2, 3] graph
      expectEdges [(2, 3)] graph

  describe "deeps" $ do
    it "Should add multiple deep nodes to graphing" $ do
      let graph :: Graphing Int
          graph = Graphing.deeps [5, 6, 7] <> Graphing.edge 2 3
      expectDirect [] graph
      expectDeps [5, 6, 7, 2, 3] graph
      expectEdges [(2, 3)] graph

  describe "direct" $ do
    it "Should add a direct node to the graph" $ do
      let graph :: Graphing Int
          graph = Graphing.direct 1 <> Graphing.edge 1 2
      expectDirect [1] graph
      expectDeps [1, 2] graph
      expectEdges [(1, 2)] graph

  describe "directs" $ do
    it "Should add multiple direct nodes to the graph" $ do
      let graph :: Graphing Int
          graph = Graphing.directs [1, 2] <> Graphing.edges [(1, 2), (1, 3)]
      expectDirect [1, 2] graph
      expectDeps [1, 2, 3] graph
      expectEdges [(1, 2), (1, 3)] graph

shrinkingSpec :: Spec
shrinkingSpec = context "Shrinking" $ do
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

          -- 1 -> 2 -> 3 -> 4
          graph'' :: Graphing Int
          graph'' = Graphing.shrink (/= 1) (Graphing.direct 1 <> Graphing.edges [(1, 2), (2, 3), (3, 4)])

      expectDirect [] graph'
      expectDeps [1, 4, 5] graph'
      expectEdges [(1, 4), (4, 5)] graph'

      expectDirect [2] graph''
      expectDeps [2, 3, 4] graph''
      expectEdges [(2, 3), (3, 4)] graph''

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

    it "should not promote any deps to direct" $ do
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

      expectDirect [1] graph'
      expectDeps [1, 2, 4, 6, 7, 8] graph'
      expectEdges [(1, 2), (2, 4), (2, 6), (2, 7)] graph'

    it "doesn't preserve nodes that reference themselves when shrinkingSpec" $ do
      -- 1 -> 2 -> 3 -> 4 -> 2
      -- 5

      let graph :: Graphing Int
          graph =
            Graphing.edges
              [ (1, 2)
              , (2, 3)
              , (3, 4)
              , (3, 2)
              ]
              <> Graphing.directs [1, 5]

          graph' = Graphing.shrinkWithoutPromotionToDirect (== 5) graph

      expectDirect [5] graph'
      expectDeps [5] graph'
      expectEdges [] graph'

getRootsOfSpec :: Spec
getRootsOfSpec =
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

pruneUnreachableSpec :: Spec
pruneUnreachableSpec =
  describe "pruneUnreachable" $ do
    it "A graph with no direct nodes should be empty after prune" $ do
      let graph :: Graphing Int
          graph = Graphing.deeps [1, 2, 3] <> Graphing.edges [(1, 2), (3, 2)]
      Graphing.pruneUnreachable graph `shouldBe` mempty

    it "A graph should remove unreachable nodes" $ do
      -- 1 -> 2 -> 3
      -- 4
      --           5 (deep)
      let graph :: Graphing Int
          graph =
            Graphing.directs [1, 4]
              <> Graphing.edges [(1, 2), (2, 3), (5, 5)]
              <> Graphing.deep 5
          pruned = Graphing.pruneUnreachable graph

      expectDeps [1, 2, 3, 4] pruned
      expectEdges [(1, 2), (2, 3)] pruned

shrinkRootsSpec :: Spec
shrinkRootsSpec =
  describe "shrinkRoots" $ do
    it "should remove direct nodes" $ do
      let graph :: Graphing Int
          graph = Graphing.directs [1] <> Graphing.edges [(1, 2), (1, 3), (2, 4), (3, 6)]
          graph' = Graphing.shrinkRoots graph

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

splitGraphSpec :: Spec
splitGraphSpec =
  describe "splitGraph" $ do
    it "Returns Nothing for a graphing with a cycle" $ do
      let am :: AM.AdjacencyMap Int
          am = AM.edge 1 1
      splitGraph am `shouldBe` Nothing
    it "Should split graphs into unconnected subgraphs" $ do
      let graph :: AM.AdjacencyMap Int
          graph = AM.edges [(1, 2), (2, 3), (4, 5)]
          subGraph1 = AM.edges [(1, 2), (2, 3)]
          subGraph2 = AM.edge 4 5
      maybe (fail "") (`shouldMatchList` [subGraph1, subGraph2]) (splitGraph graph)

stripRootSpec :: Spec
stripRootSpec =
  describe "stripRoot" $ do
    let graph :: Graphing Int
        graph = Graphing.directs [1] <> Graphing.edges [(1, 2), (1, 3), (2, 4), (3, 6)]
        graph' = Graphing.stripRoot graph

    it "should promote immediate children as direct nodes" $ do
      expectDirect [2, 3] graph'

    it "should preserve current root nodes in the graphing as nodes" $ do
      expectDeps [1, 2, 3, 4, 6] graph'

    it "should preserve edges of current root nodes in the graphing" $ do
      expectEdges [(1, 2), (1, 3), (2, 4), (3, 6)] graph'

promoteToDirectSpec :: Spec
promoteToDirectSpec =
  describe "promoteToDirect" $ do
    it "should promote nodes to direct" $ do
      let graph :: Graphing Int
          graph = Graphing.promoteToDirect (< 5) (unfold [10] (\x -> if x > 0 then [x - 2] else []) id)
      expectDirect [0, 2, 4, 10] graph
      expectDeps [10, 8, 6, 4, 2, 0] graph
      expectEdges [(10, 8), (8, 6), (6, 4), (4, 2), (2, 0)] graph

hasPredecessorsSpec :: Spec
hasPredecessorsSpec = do
  describe "hasPredecessors" $ do
    it "should report False when node has no predecessors" $ do
      --  1 -> 2 -> 3 -> 4
      let graph :: Graphing Int = Graphing.directs [1] <> Graphing.edges [(1, 2), (2, 3), (3, 4)]
      hasPredecessors graph 1 `shouldBe` False

    it "should report True when node has predecessors" $ do
      --  1 -> 2 -> 3 -> 4
      let graph :: Graphing Int = Graphing.directs [1, 2] <> Graphing.edges [(1, 2), (2, 3), (3, 4)]
      hasPredecessors graph 2 `shouldBe` True

subGraphOfSpec :: Spec
subGraphOfSpec = do
  --   1 -> 2
  --    \    \
  --     3    \
  --      \    4
  --       6
  let graph1 :: Graphing Int = Graphing.directs [1, 2] <> Graphing.edges [(1, 2), (1, 3), (2, 4), (3, 6)]

  --     -- 1 -> 2 -> 5 -> 6
  --     --      \    \
  --     --       \    7
  --     -- 3 ----> 4
  let graph2 :: Graphing Int = Graphing.edges [(1, 2), (3, 4), (2, 4), (2, 5), (5, 7), (5, 6)] <> Graphing.directs [1, 3]

  describe "subGraphOf" $ do
    it "should return subGraph of 0 for graph1 correctly" $ do
      let graphSub0 = subGraphOf 0 graph1
      expectEdges [] graphSub0
      expectDeps [] graphSub0
      expectDirect [] graphSub0

    it "should return subGraph of 1 for graph1 correctly" $ do
      let graphSub1 = subGraphOf 1 graph1
      graphSub1 `shouldBe` graph1

    it "should return subGraph of 2 for graph1 correctly" $ do
      let graphSub2 = subGraphOf 2 graph1
      expectEdges [(2, 4)] graphSub2
      expectDeps [2, 4] graphSub2
      expectDirect [2] graphSub2

    it "should return subGraph of 3 for graph1 correctly" $ do
      let graphSub3 = subGraphOf 3 graph1
      expectEdges [(3, 6)] graphSub3
      expectDeps [3, 6] graphSub3
      expectDirect [] graphSub3

    it "should return subGraph of 5 for graph2 correctly" $ do
      let graph2Sub5 = subGraphOf 5 graph2
      expectEdges [(5, 7), (5, 6)] graph2Sub5
      expectDeps [5, 6, 7] graph2Sub5
      expectDirect [] graph2Sub5

reachableSuccessorsWithConditionSpec :: Spec
reachableSuccessorsWithConditionSpec = do
  --   1 -> 2
  --    \    \
  --     3    \
  --      \    4
  --       6    \
  --             5
  let graph :: Graphing Int = Graphing.directs [1, 2] <> Graphing.edges [(1, 2), (1, 3), (2, 4), (3, 6), (4, 5)]

  --   1 -> 2 -> 3
  --        |    |
  --        5 <- 4
  let cyclicGraph :: Graphing Int = Graphing.directs [1] <> Graphing.edges [(1, 2), (2, 3), (3, 4), (4, 5), (5, 2)]

  let condition :: Int -> Set Int -> Bool
      condition x excluded = x `Set.notMember` excluded
  let emptySet :: Set Int
      emptySet = Set.fromList []

  describe "reachableSuccessorsWithCondition" $ do
    it "should return the successors of 1 excluding 2 and its successors" $ do
      let excludedVals = Set.fromList [2]
      reachableSuccessorsWithCondition (edgesList graph) 1 condition excludedVals emptySet `shouldBe` Set.fromList [3, 6]

    it "should return no successors" $ do
      let excludedVals = Set.fromList [2, 3]
      reachableSuccessorsWithCondition (edgesList graph) 1 condition excludedVals emptySet `shouldBe` emptySet

    it "should return successors of 1 when the graph is cyclic" $ do
      reachableSuccessorsWithCondition (edgesList cyclicGraph) 1 condition emptySet emptySet `shouldBe` Set.fromList [2, 3, 4, 5]

data SimpleDep = SimpleDep
  { name :: Text
  , colorTag :: Text
  , colors :: Set Text
  }
  deriving (Eq, Ord, Show)

colorSpec :: Spec
colorSpec = do
  --   d1 -> d2
  --    \     \
  --     d3    \
  --           d4
  let d1 = SimpleDep "dep1" "blue" mempty
  let d2 = SimpleDep "dep2" "green" mempty
  let d3 = SimpleDep "dep3" "red" mempty
  let d4 = SimpleDep "dep4" "yellow" mempty
  let graph :: Graphing SimpleDep = Graphing.directs [d1, d2] <> Graphing.edges [(d1, d2), (d1, d3), (d2, d4)]

  let updateColors :: Set Text -> SimpleDep -> SimpleDep
      updateColors newColor dep = dep{colors = newColor}

  let extractColors :: SimpleDep -> Set Text
      extractColors = colors

  let extractColorTag :: SimpleDep -> Text
      extractColorTag = colorTag

  let depsToColor = Set.fromList ["blue", "green", "yellow"]

  describe "color" $ do
    it "should color d1, d2, and d4 with blue" $ do
      --   d1(blue) -> d2(blue)
      --    \           \
      --     d3          \
      --                d4(blue)
      let coloredD1 = SimpleDep "dep1" "blue" $ Set.fromList ["blue"]
      let coloredD2 = SimpleDep "dep2" "green" $ Set.fromList ["blue"]
      let unColoredD3 = SimpleDep "dep3" "red" mempty
      let coloredD4 = SimpleDep "dep4" "yellow" $ Set.fromList ["blue"]
      let graph' :: Graphing SimpleDep = Graphing.directs [coloredD1, coloredD2] <> Graphing.edges [(coloredD1, coloredD2), (coloredD1, unColoredD3), (coloredD2, coloredD4)]

      color graph extractColors updateColors d1 extractColorTag depsToColor `shouldBe` graph'

spec :: Spec
spec = do
  unfoldSpec

  accessingElementsSpec

  addingNodesSpec

  shrinkingSpec

  pruneUnreachableSpec

  getRootsOfSpec

  shrinkRootsSpec

  splitGraphSpec

  stripRootSpec

  promoteToDirectSpec

  hasPredecessorsSpec

  subGraphOfSpec

  reachableSuccessorsWithConditionSpec

  colorSpec
