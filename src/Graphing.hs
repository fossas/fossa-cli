-- | A graph augmented with a set of "direct" vertices
--
-- Graphings can be built with the 'direct' and 'edge' primitives.
--
-- For simple (non-cyclic) graphs, try 'unfold'. For graphs with only direct dependencies, try 'fromList'
--
-- For describing complex graphs, see the 'Effect.Grapher' effect.
module Graphing (
  -- * Graphing type
  Graphing (..),
  empty,
  size,
  direct,
  edge,
  deep,

  -- * Manipulating a Graphing
  gmap,
  gtraverse,
  induceJust,
  Graphing.filter,
  pruneUnreachable,
  stripRoot,
  promoteToDirect,

  -- * Conversions
  fromAdjacencyMap,
  fromList,
  toList,
  unfold,
) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AMA
import Algebra.Graph.AdjacencyMap.Extra qualified as AME
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set

-- | A @Graphing ty@ is a graph of nodes with type @ty@.
--
-- Nodes (via 'direct') and edges (via 'edge') added to the graph are
-- automatically deduplicated using the 'Ord' instance of @ty@.
--
-- Typically, when consuming a Graphing, we only care about nodes in the graph
-- reachable from 'graphingDirect'
data Graphing ty = Graphing
  { graphingDirect :: Set ty
  , graphingAdjacent :: AdjacencyMap ty
  }
  deriving (Eq, Ord, Show)

instance Ord ty => Semigroup (Graphing ty) where
  graphing <> graphing' =
    Graphing
      (graphingDirect graphing `Set.union` graphingDirect graphing')
      (graphingAdjacent graphing `AM.overlay` graphingAdjacent graphing')

instance Ord ty => Monoid (Graphing ty) where
  mempty = Graphing Set.empty AM.empty

-- | Transform a Graphing by applying a function to each of its vertices.
--
-- Graphing isn't a lawful 'Functor', so we don't provide an instance.
gmap :: (Ord ty, Ord ty') => (ty -> ty') -> Graphing ty -> Graphing ty'
gmap f gr = gr{graphingDirect = direct', graphingAdjacent = adjacent'}
  where
    direct' = Set.map f (graphingDirect gr)
    adjacent' = AM.gmap f (graphingAdjacent gr)

-- | Map each element of the Graphing to an action, evaluate the actions, and
-- collect the results.
--
-- Graphing isn't a lawful 'Traversable', so we don't provide an instance.
gtraverse :: (Ord b, Applicative f) => (a -> f b) -> Graphing a -> f (Graphing b)
gtraverse f Graphing{graphingDirect, graphingAdjacent} = Graphing <$> newSet <*> newAdjacent
  where
    -- newSet :: f (Set b)
    newSet = fmap Set.fromList . traverse f . Set.toList $ graphingDirect

    -- newAdjacent :: f (AM.AdjacencyMap b)
    newAdjacent = AME.gtraverse f graphingAdjacent

-- | Like 'AM.induceJust', but for Graphings
induceJust :: Ord a => Graphing (Maybe a) -> Graphing a
induceJust gr = gr{graphingDirect = direct', graphingAdjacent = adjacent'}
  where
    direct' = Set.fromList . catMaybes . Set.toList $ graphingDirect gr
    adjacent' = AM.induceJust (graphingAdjacent gr)

-- | Filter Graphing elements
filter :: (ty -> Bool) -> Graphing ty -> Graphing ty
filter f gr = gr{graphingDirect = direct', graphingAdjacent = adjacent'}
  where
    direct' = Set.filter f (graphingDirect gr)
    adjacent' = AM.induce f (graphingAdjacent gr)

-- | The empty Graphing
empty :: Graphing ty
empty = Graphing Set.empty AM.empty

-- | Determines the number of nodes in the graph ("reachable" or not)
size :: Graphing ty -> Int
size = AM.vertexCount . graphingAdjacent

-- | Strip all items from the direct set, promote their immediate children to direct items
stripRoot :: Ord ty => Graphing ty -> Graphing ty
stripRoot gr = gr{graphingDirect = direct'}
  where
    roots = Set.toList $ graphingDirect gr
    edgeSet root = AM.postSet root $ graphingAdjacent gr
    direct' = Set.unions $ map edgeSet roots

-- | Add a direct dependency to this Graphing
direct :: Ord ty => ty -> Graphing ty -> Graphing ty
direct dep gr = gr{graphingDirect = direct', graphingAdjacent = adjacent'}
  where
    direct' = Set.insert dep (graphingDirect gr)
    adjacent' = AM.overlay (AM.vertex dep) (graphingAdjacent gr)

-- | Mark dependencies that pass a predicate as direct dependencies.
-- Dependencies that are already marked as "direct" are unaffected.
promoteToDirect :: Ord ty => (ty -> Bool) -> Graphing ty -> Graphing ty
promoteToDirect f gr = gr{graphingDirect = direct'}
  where
    direct' = foldr Set.insert (graphingDirect gr) vertices
    vertices = List.filter f $ AM.vertexList (graphingAdjacent gr)

-- | Add an edge between two nodes in this Graphing
edge :: Ord ty => ty -> ty -> Graphing ty -> Graphing ty
edge parent child gr = gr{graphingAdjacent = adjacent'}
  where
    adjacent' = AM.overlay (AM.edge parent child) (graphingAdjacent gr)

-- | Adds a node to this graph as a deep dependency.
deep :: Ord ty => ty -> Graphing ty -> Graphing ty
deep dep gr = gr{graphingAdjacent = adjacent'}
  where
    adjacent' = AM.overlay (AM.vertex dep) (graphingAdjacent gr)

-- | @unfold direct getDeps toDependency@ unfolds a graph, given:
--
-- - The @direct@ dependencies in the graph
--
-- - A way to @getDeps@ for a dependency
--
-- - A way to convert a dependency @toDependency@
--
-- __Unfold does not work for recursive inputs__
unfold :: forall dep res. Ord res => [dep] -> (dep -> [dep]) -> (dep -> res) -> Graphing res
unfold seed getDeps toDependency =
  Graphing
    { graphingDirect = Set.fromList (map toDependency seed)
    , graphingAdjacent = AM.vertices (map toDependency seed) `AM.overlay` AM.edges [(toDependency parentDep, toDependency childDep) | (parentDep, childDep) <- edgesFrom seed]
    }
  where
    edgesFrom :: [dep] -> [(dep, dep)]
    edgesFrom nodes = do
      node <- nodes
      let children = getDeps node
      map (node,) children ++ edgesFrom children

-- | Remove unreachable vertices from the graph
--
-- A vertex is reachable if there's a path from the "direct" vertices to that vertex
pruneUnreachable :: Ord ty => Graphing ty -> Graphing ty
pruneUnreachable gr = gr{graphingAdjacent = AM.induce (`Set.member` reachable) (graphingAdjacent gr)}
  where
    reachable = Set.fromList $ AMA.dfs (Set.toList (graphingDirect gr)) (graphingAdjacent gr)

-- | Build a graphing from a list, where all list elements are treated as direct
-- dependencies
fromList :: Ord ty => [ty] -> Graphing ty
fromList nodes = Graphing (Set.fromList nodes) (AM.vertices nodes)

-- | Wrap an AdjacencyMap as a Graphing
fromAdjacencyMap :: AM.AdjacencyMap ty -> Graphing ty
fromAdjacencyMap = Graphing Set.empty

-- | Get the list of nodes in the Graphing.
toList :: Graphing ty -> [ty]
toList Graphing{graphingAdjacent} = AM.vertexList graphingAdjacent
