-- | A graph augmented with a set of "direct" vertices
--
-- Graphings can be built with the 'direct' and 'edge' primitives.
--
-- For simple (non-cyclic) graphs, try 'unfold'. For graphs with only direct dependencies, try 'fromList'
--
-- For describing complex graphs, see the 'Effect.Grapher' effect.
module Graphing
  ( -- * Graphing type
    Graphing(..)
  , empty
  , direct
  , edge

  -- * Manipulating a Graphing
  , gmap
  , filter
  , pruneUnreachable
  , stripRoot

  -- * Building simple Graphings
  , fromList
  , unfold
  ) where

import Prologue hiding (empty, filter, parent)

import           Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA
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

-- | Filter Graphing elements
filter :: (ty -> Bool) -> Graphing ty -> Graphing ty
filter f gr = gr { graphingDirect = direct', graphingAdjacent = adjacent' }
  where
    direct' = S.filter f (graphingDirect gr)
    adjacent' = AM.induce f (graphingAdjacent gr)

-- | The empty Graphing
empty :: Graphing ty
empty = Graphing S.empty AM.empty

-- | Strip all items from the direct set, promote their immediate children to direct items
stripRoot :: Ord ty => Graphing ty -> Graphing ty
stripRoot gr = gr { graphingDirect = direct' }
  where
  roots = S.toList $ graphingDirect gr
  edgeSet root = AM.postSet root $ graphingAdjacent gr
  direct' = S.unions $ map edgeSet roots

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

-- | Build a graphing from a list, where all list elements are treated as direct
-- dependencies
fromList :: Ord ty => [ty] -> Graphing ty
fromList nodes = Graphing (S.fromList nodes) (AM.vertices nodes)

-- | Remove unreachable vertices from the graph
--
-- A vertex is reachable if there's a path from the "direct" vertices to that vertex
pruneUnreachable :: Ord ty => Graphing ty -> Graphing ty
pruneUnreachable gr = gr { graphingAdjacent = AM.induce (`S.member` reachable) (graphingAdjacent gr) }
  where
    reachable = S.fromList $ AMA.dfs (S.toList (graphingDirect gr)) (graphingAdjacent gr)
