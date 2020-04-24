module App.Fossa.Analyze.GraphMangler
  ( graphingToGraph
  ) where

import Prologue hiding (parent)

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.ToGraph (dfs)
import Control.Algebra
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import App.Fossa.Analyze.GraphBuilder
import qualified App.Fossa.Analyze.Graph as G
import DepTypes
import Graphing (Graphing(..))

graphingToGraph :: Graphing Dependency -> G.Graph
graphingToGraph graphing = run . evalGraphBuilder G.empty $ do
  let depAmap = graphingAdjacent graphing
      depDirect = S.toList (graphingDirect graphing)

      nodes = dfs depDirect depAmap

  refs <- M.fromList <$> traverse addingNode nodes

  traverse_ (visitNode refs depAmap) nodes

  traverse_ (\dep -> traverse_ addDirect (M.lookup dep refs)) depDirect

  where

  -- add a node with GraphBuilder
  addingNode :: Has GraphBuilder sig m => Dependency -> m (Dependency, G.DepRef)
  addingNode k = do
    ref <- addNode k
    pure (k, ref)

  -- visit a node, adding edges between it and all of its dependencies
  visitNode :: Has GraphBuilder sig m => Map Dependency G.DepRef -> AdjacencyMap Dependency -> Dependency -> m ()
  visitNode refs amap node = traverse_ (visitEdge refs node) (S.toList $ AM.postSet node amap)

  -- visit an edge by adding it to the graph
  visitEdge :: Has GraphBuilder sig m => Map Dependency G.DepRef -> Dependency -> Dependency -> m ()
  visitEdge refs parent child = do
    let edgeRefs = do
          parentRef <- M.lookup parent refs
          childRef <- M.lookup child refs
          pure (parentRef, childRef)

    traverse_ (uncurry addEdge) edgeRefs
