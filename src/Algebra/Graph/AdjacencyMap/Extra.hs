module Algebra.Graph.AdjacencyMap.Extra (
  gtraverse,
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
