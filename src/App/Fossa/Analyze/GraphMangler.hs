module App.Fossa.Analyze.GraphMangler (
  graphingToGraph,
) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Algebra
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import App.Fossa.Analyze.Graph qualified as G
import App.Fossa.Analyze.GraphBuilder
import DepTypes
import Graphing (Graphing)
import Graphing qualified

graphingToGraph :: Graphing Dependency -> G.Graph
graphingToGraph graphing = run . evalGraphBuilder G.empty $ do
  let depAmap = Graphing.toAdjacencyMap graphing
      depDirect = Graphing.directList graphing

      nodes = Graphing.vertexList graphing

  refs <- Map.fromList <$> traverse addingNode nodes

  traverse_ (visitNode refs depAmap) nodes

  traverse_ (\dep -> traverse_ addDirect (Map.lookup dep refs)) depDirect
  where
    -- add a node with GraphBuilder
    addingNode :: Has GraphBuilder sig m => Dependency -> m (Dependency, G.DepRef)
    addingNode k = do
      ref <- addNode k
      pure (k, ref)

    -- visit a node, adding edges between it and all of its dependencies
    visitNode :: Has GraphBuilder sig m => Map Dependency G.DepRef -> AdjacencyMap Dependency -> Dependency -> m ()
    visitNode refs amap node = traverse_ (visitEdge refs node) (Set.toList $ AM.postSet node amap)

    -- visit an edge by adding it to the graph
    visitEdge :: Has GraphBuilder sig m => Map Dependency G.DepRef -> Dependency -> Dependency -> m ()
    visitEdge refs parent child = do
      let edgeRefs = do
            parentRef <- Map.lookup parent refs
            childRef <- Map.lookup child refs
            pure (parentRef, childRef)

      traverse_ (uncurry addEdge) edgeRefs
