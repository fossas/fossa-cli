module Graphing
  ( Graphing(..)
  , empty
  , direct
  , edge
  , gmap

  , unfold
  ) where

import Prologue hiding (empty, map, parent)

import           Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Set as S

-- | A @Graphing ty@ is a graph of nodes with type @ty@.
--
-- Nodes (via 'direct') and edges (via 'edge') added to the graph are
-- automatically deduplicated using the 'Ord' instance of @ty@.
--
-- Typically, when consuming a Graphing, we only care about nodes in the graph
-- reachable from 'graphingDirect'
data Graphing ty = Graphing
  { graphingDirect   :: Set ty
  , graphingAdjacent :: AdjacencyMap ty
  } deriving (Eq, Ord, Show, Generic)

-- | Transform a Graphing by applying a function to each of its vertices.
--
-- Graphing isn't a lawful 'Functor', so we don't provide an instance.
gmap :: (Ord ty, Ord ty') => (ty -> ty') -> Graphing ty -> Graphing ty'
gmap f gr = gr { graphingDirect = direct', graphingAdjacent = adjacent' }
  where
  direct' = S.map f (graphingDirect gr)
  adjacent' = AM.gmap f (graphingAdjacent gr)

-- | The empty Graphing
empty :: Graphing ty
empty = Graphing S.empty AM.empty

-- | Add a direct dependency to this Graphing
direct :: Ord ty => ty -> Graphing ty -> Graphing ty
direct dep gr = gr { graphingDirect = direct', graphingAdjacent = adjacent' }
  where
  direct' = S.insert dep (graphingDirect gr)
  adjacent' = AM.overlay (AM.vertex dep) (graphingAdjacent gr)

-- | Add an edge between two nodes in this Graphing
edge :: Ord ty => ty -> ty -> Graphing ty -> Graphing ty
edge parent child gr = gr { graphingAdjacent = adjacent' }
  where
  adjacent' = AM.overlay (AM.edge parent child) (graphingAdjacent gr)

-- | @unfold direct getDeps toDependency@ unfolds a graph, given:
--
-- - The @direct@ dependencies in the graph
--
-- - A way to @getDeps@ for a dependency
--
-- - A way to convert a dependency @toDependency@
--
-- __Unfold does not work for recursive inputs__
unfold :: Ord res => [dep] -> (dep -> [dep]) -> (dep -> res) -> Graphing res
unfold seed getDeps toDependency = foldr addNode empty seed
  where
  addNode dep gr = direct res (foldr (edge res . toDependency) gr children)
    where
    children = getDeps dep
    res = toDependency dep
