-- | A graph (Algebra.Graph.AdjacencyMap) augmented with the notion of "direct" vertices.
--
-- Graphings can be built with the 'direct', 'edge', 'deep', 'directs', ... primitives.
--
-- For simple (non-cyclic) graphs, try 'unfold'. For graphs with only direct dependencies, try 'fromList'
--
-- Most commonly, Graphings are built by combining sub-graphs with (<>), e.g.,
--
-- @
--     myCoolGraph = directs myListOfDirectNodes <> edges myListOfEdges
-- @
--
-- For describing complex graphs, see the 'Effect.Grapher' effect.
module Graphing (
  -- * Graphing type
  Graphing (..),
  Node (..),
  empty,
  size,
  direct,
  directs,
  edge,
  edges,
  deep,
  deeps,

  -- * Accessing graph elements
  directList,
  vertexList,
  toAdjacencyMap,

  -- * Manipulating a Graphing
  gmap,
  gtraverse,
  induce,
  induceJust,
  filter,
  shrink,
  shrinkSingle,
  shrinkWithoutPromotionToDirect,
  pruneUnreachable,
  stripRoot,
  promoteToDirect,
  shrinkRoots,

  -- * Conversions
  fromAdjacencyMap,
  fromList,
  toList,
  unfold,
  unfoldDeep,
) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AMA
import Algebra.Graph.AdjacencyMap.Extra qualified as AME
import Data.Bifunctor (bimap)
import Data.Set qualified as Set
import Prelude hiding (filter)
import Prelude qualified

-- | A @Graphing ty@ is a graph of nodes with type @ty@.
--
-- Nodes (via 'direct') and edges (via 'edge') added to the graph are
-- automatically deduplicated using the 'Ord' instance of @ty@.
--
-- Typically, when consuming a Graphing, we only care about nodes in the graph
-- reachable from 'directList'
newtype Graphing ty = Graphing {unGraphing :: AdjacencyMap (Node ty)}
  deriving (Eq, Ord, Show)

data Node a = Root | Node a
  deriving (Eq, Ord, Show)

instance Functor Node where
  fmap _ Root = Root
  fmap f (Node a) = Node (f a)

instance Foldable Node where
  foldMap _ Root = mempty
  foldMap f (Node a) = f a

instance Traversable Node where
  traverse _ Root = pure Root
  traverse f (Node a) = Node <$> f a

instance Ord ty => Semigroup (Graphing ty) where
  Graphing graphing <> Graphing graphing' = Graphing (AM.overlay graphing graphing')

instance Ord ty => Monoid (Graphing ty) where
  mempty = Graphing AM.empty

-- | Transform a Graphing by applying a function to each of its vertices.
--
-- Graphing isn't a lawful 'Functor', so we don't provide an instance.
gmap :: (Ord ty, Ord ty') => (ty -> ty') -> Graphing ty -> Graphing ty'
gmap f = Graphing . AM.gmap (fmap f) . unGraphing

-- | Map each element of the Graphing to an action, evaluate the actions, and
-- collect the results.
--
-- Graphing isn't a lawful 'Traversable', so we don't provide an instance.
gtraverse :: (Ord b, Applicative f) => (a -> f b) -> Graphing a -> f (Graphing b)
gtraverse f = fmap Graphing . AME.gtraverse (traverse f) . unGraphing

-- | Filter Graphing elements. Alias for 'filter' to match the AdjacencyMap naming
induce :: (ty -> Bool) -> Graphing ty -> Graphing ty
induce = filter

-- | Like 'AM.induceJust', but for Graphings
induceJust :: Ord a => Graphing (Maybe a) -> Graphing a
induceJust = Graphing . AM.induceJust . AM.gmap sequenceA . unGraphing

-- | Filter Graphing elements
filter :: forall ty. (ty -> Bool) -> Graphing ty -> Graphing ty
filter f = Graphing . AM.induce f' . unGraphing
  where
    f' :: Node ty -> Bool
    f' Root = True
    f' (Node a) = f a

-- | Filter vertices in a Graphing, preserving the overall structure by rewiring edges through deleted vertices.
--
-- For example, given the graph @1 -> 2 -> 3 -> 4@ and applying @shrink (/= 3)@, we return the graph
-- @1 -> 2 -> 4@
shrink :: forall a. Ord a => (a -> Bool) -> Graphing a -> Graphing a
shrink f = Graphing . AME.shrink f' . unGraphing
  where
    f' :: Node a -> Bool
    f' Root = True
    f' (Node a) = f a

-- | Unlike @shrink@ when root vertices are deleted, their successor are not promoted as direct.
shrinkWithoutPromotionToDirect :: forall a. Ord a => (a -> Bool) -> Graphing a -> Graphing a
shrinkWithoutPromotionToDirect f gr = foldl withoutEdge shrinkedGraph jumpedDirects
  where
    shrinkedGraph :: Graphing a
    shrinkedGraph = shrink f gr

    jumpedDirects :: [a]
    jumpedDirects =
      Set.toList $
        Set.difference
          (Set.fromList . directList $ shrinkedGraph)
          (Set.fromList . directList $ gr)

    withoutEdge :: Graphing a -> a -> Graphing a
    withoutEdge g n = Graphing . AM.removeEdge Root (Node n) $ unGraphing g

-- | Delete a vertex in a Grahing, preserving the overall structure by rewiring edges through the delted vertex.
--
-- For example, given the graph @1 -> 2 -> 3 -> 4@ and applying @shrinkSingle 3@, we return the graph
-- @1 -> 2 -> 4@
shrinkSingle :: Ord a => a -> Graphing a -> Graphing a
shrinkSingle vert = Graphing . AME.shrinkSingle (Node vert) . unGraphing

-- | The empty Graphing
empty :: Graphing ty
empty = Graphing AM.empty

-- | Determines the number of nodes in the graph ("reachable" or not)
size :: Graphing ty -> Int
size = length . Set.filter ignoreRoot . AM.vertexSet . unGraphing
  where
    ignoreRoot :: Node a -> Bool
    ignoreRoot Root = False
    ignoreRoot _ = True

-- | Strip all items from the direct set, promote their immediate children to direct items
stripRoot :: forall ty. Ord ty => Graphing ty -> Graphing ty
stripRoot (Graphing gr) = Graphing $ AM.overlay newDirectEdges (AM.removeVertex Root gr)
  where
    currentDirect :: [Node ty]
    currentDirect = Set.toList $ AM.postSet Root gr

    newDirect :: [Node ty]
    newDirect = Set.toList . Set.unions $ map (`AM.postSet` gr) currentDirect

    newDirectEdges :: AM.AdjacencyMap (Node ty)
    newDirectEdges = AM.edges $ map (Root,) newDirect

-- | Shrinks all root nodes.
-- Remove current direct nodes. It will promote their immediate children as directs.
-- Unlike @stripRoot@, it removes them from graphing, instead of preserving them (as node), and their edges.
shrinkRoots :: forall ty. Ord ty => Graphing ty -> Graphing ty
shrinkRoots (Graphing gr) = Graphing $ foldr AME.shrinkSingle gr currentDirect
  where
    currentDirect :: [Node ty]
    currentDirect = Set.toList $ AM.postSet Root gr

-- | Add a direct node to this Graphing
direct :: Ord ty => ty -> Graphing ty
direct node = Graphing (AM.edge Root (Node node))

-- | Add several direct nodes to this Graphing
directs :: Ord ty => [ty] -> Graphing ty
directs nodes = Graphing (AM.edges [(Root, Node node) | node <- nodes])

-- | Mark dependencies that pass a predicate as direct dependencies.
-- Dependencies that are already marked as "direct" are unaffected.
promoteToDirect :: forall ty. Ord ty => (ty -> Bool) -> Graphing ty -> Graphing ty
promoteToDirect f gr = gr <> directs matchingVertices
  where
    matchingVertices :: [ty]
    matchingVertices = Prelude.filter f (vertexList gr)

-- | Get a list of direct vertices in the Graphing
directList :: Ord ty => Graphing ty -> [ty]
directList (Graphing gr) = [node | Node node <- Set.toList (AM.postSet Root gr)]

-- | Get a list of vertices in the Graphing
vertexList :: Graphing ty -> [ty]
vertexList gr = [node | Node node <- AM.vertexList (unGraphing gr)]

-- | Convert to the underlying AdjacencyMap (without the Root element)
toAdjacencyMap :: Ord ty => Graphing ty -> AM.AdjacencyMap ty
toAdjacencyMap = AM.induceJust . AM.gmap convert . unGraphing
  where
    convert :: Node ty -> Maybe ty
    convert Root = Nothing
    convert (Node a) = Just a

-- | Build a Graphing containing a single edge between two nodes
edge :: Ord ty => ty -> ty -> Graphing ty
edge parent child = Graphing (AM.edge (Node parent) (Node child))

-- | Build a Graphing containing several edges
edges :: Ord ty => [(ty, ty)] -> Graphing ty
edges = Graphing . AM.edges . map (bimap Node Node)

-- | Add a single deep node to the graphing
deep :: ty -> Graphing ty
deep = Graphing . AM.vertex . Node

-- | Add several deep nodes to the graphing.
deeps :: Ord ty => [ty] -> Graphing ty
deeps = Graphing . AM.vertices . map Node

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
unfold seed getDeps toDependency = directs directNodes <> edges builtEdges
  where
    directNodes :: [res]
    directNodes = map toDependency seed

    builtEdges :: [(res, res)]
    builtEdges = map (bimap toDependency toDependency) (edgesFrom seed)

    edgesFrom :: [dep] -> [(dep, dep)]
    edgesFrom nodes = do
      node <- nodes
      let children = getDeps node
      map (node,) children ++ edgesFrom children

-- | @unfoldDeep dep getDeps toDependency@ unfolds a graph, given:
--
-- - The @deep@ dependencies in the graph
--
-- - A way to @getDeps@ for a dependency
--
-- - A way to convert a dependency @toDependency@
--
-- __unfoldDeep does not work for recursive inputs__
-- __unfoldDeep marks all dependencies as deeps__
unfoldDeep :: forall dep res. Ord res => [dep] -> (dep -> [dep]) -> (dep -> res) -> Graphing res
unfoldDeep seed getDeps toDependency = deeps directNodes <> edges builtEdges
  where
    directNodes :: [res]
    directNodes = map toDependency seed

    builtEdges :: [(res, res)]
    builtEdges = map (bimap toDependency toDependency) (edgesFrom seed)

    edgesFrom :: [dep] -> [(dep, dep)]
    edgesFrom nodes = do
      node <- nodes
      let children = getDeps node
      map (node,) children ++ edgesFrom children

-- | Remove unreachable vertices from the graph
--
-- A vertex is reachable if there's a path from the "direct" vertices to that vertex
pruneUnreachable :: forall ty. Ord ty => Graphing ty -> Graphing ty
pruneUnreachable (Graphing gr) = Graphing (AM.induce keepPredicate gr)
  where
    directNodes :: [Node ty]
    directNodes = Set.toList $ AM.postSet Root gr

    reachableNodes :: Set.Set (Node ty)
    reachableNodes = Set.fromList $ AMA.dfs directNodes gr

    keepPredicate :: Node ty -> Bool
    keepPredicate Root = True
    keepPredicate (Node ty) = Set.member (Node ty) reachableNodes

-- | Build a graphing from a list, where all list elements are treated as direct
-- dependencies
--
-- Alias for 'directs'
fromList :: Ord ty => [ty] -> Graphing ty
fromList = directs

-- | Wrap an AdjacencyMap as a Graphing
--
-- All nodes in the resulting Graphing are considered indirect/"deep"
fromAdjacencyMap :: Ord ty => AM.AdjacencyMap ty -> Graphing ty
fromAdjacencyMap = Graphing . AM.gmap Node

-- | Get the list of nodes in the Graphing.
--
-- Alias for 'vertexList'
toList :: Graphing ty -> [ty]
toList = vertexList
