module Algebra.Graph.AdjacencyMap.Extra (
  gtraverse,
  shrink,
  shrinkSingle,
  splitGraph,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AMA
import Data.Set (Set)
import Data.Set qualified as Set

-- | It's 'traverse', but for graphs
--
-- It's also unlawful. 'f' might be called several times for each node in the graph
gtraverse ::
  (Applicative f, Ord b) =>
  (a -> f b) ->
  AM.AdjacencyMap a ->
  f (AM.AdjacencyMap b)
gtraverse f = fmap mkAdjacencyMap . traverse (\(a, xs) -> (,) <$> f a <*> traverse f xs) . AM.adjacencyList
  where
    mkAdjacencyMap :: Ord c => [(c, [c])] -> AM.AdjacencyMap c
    mkAdjacencyMap = AM.fromAdjacencySets . fmap (fmap Set.fromList)

-- | Filter vertices in an AdjacencyMap, preserving the overall structure by rewiring edges through deleted vertices.
--
-- For example, given the graph @1 -> 2 -> 3 -> 4@ and applying @shrink (/= 3)@, we return the graph
-- @1 -> 2 -> 4@
shrink :: Ord a => (a -> Bool) -> AM.AdjacencyMap a -> AM.AdjacencyMap a
shrink f gr = foldr shrinkSingle gr filteredOutVertices
  where
    filteredOutVertices = filter (not . f) (AM.vertexList gr)

-- | Delete a vertex in an AdjacencyMap, preserving the overall structure by rewiring edges through the delted vertex.
--
-- For example, given the graph @1 -> 2 -> 3 -> 4@ and applying @shrinkSingle 3@, we return the graph
-- @1 -> 2 -> 4@
shrinkSingle :: forall a. Ord a => a -> AM.AdjacencyMap a -> AM.AdjacencyMap a
shrinkSingle vert gr = AM.overlay (AM.removeVertex vert gr) inducedEdges
  where
    -- If @vert@ has an edge to itself, it would be added back in by @overlay@
    -- as an induced edge so delete vert from it's pre/post set
    inducedEdges :: AM.AdjacencyMap a
    inducedEdges =
      AM.edges
        [ (pre, post)
        | pre <- Set.toList . Set.delete vert $ AM.preSet vert gr
        , post <- Set.toList . Set.delete vert $ AM.postSet vert gr
        ]

-- | Split graph into distinct sibling graphs (unconnected subgraphs) via nodes
-- that have no incoming edges.  Returns 'Nothing' if graph is cyclic.
--
-- @
-- splitGraph 'edges' [(1, 2), (2, 3), (4, 5)] = ['edges' [(1, 2), (2, 3)], 'edge' 4 5]
-- @
splitGraph :: Ord a => AM.AdjacencyMap a -> Maybe [AM.AdjacencyMap a]
splitGraph gr =
  if AMA.isAcyclic gr
    then Just $ splitAcyclicGraph gr
    else Nothing

-- | The internals of splitGraph, but only for acyclic graphs.  Results for
-- cyclic graphs are unspecified.
splitAcyclicGraph :: Ord a => AM.AdjacencyMap a -> [AM.AdjacencyMap a]
splitAcyclicGraph gr =
  case AM.vertexList $ AM.induce (\x -> Set.null $ AM.preSet x gr) gr of
    [] -> [gr]
    tops -> map (`takeReachable` gr) tops

takeReachable :: forall a. Ord a => a -> AM.AdjacencyMap a -> AM.AdjacencyMap a
takeReachable x gr = AM.induce (`Set.member` vertices) gr
  where
    vertices :: Set a
    vertices = Set.fromList $ AMA.reachable gr x
