module Algebra.Graph.AdjacencyMap.Extra (
  gtraverse,
  shrink,
  shrinkSingle,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
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
    inducedEdges :: AM.AdjacencyMap a
    inducedEdges =
      AM.edges
        [ (pre, post)
        | pre <- Set.toList (AM.preSet vert gr)
        , post <- Set.toList (AM.postSet vert gr)
        ]
