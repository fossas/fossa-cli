{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze.Graph (
  Graph (),
  DepRef (),
  empty,
  addDirect,
  addEdge,
  addNode,
  graphAssocs,
  graphDeps,
  graphDirect,
) where

import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Sequence qualified as S

import DepTypes

-- | Opaque reference to a dependency in the graph. Used for adding edges to the graph (See: 'addEdge')
newtype DepRef = DepRef {unDepRef :: Int} deriving (Eq, Ord, Show)

-- | A Graph of dependencies. See 'empty', 'addNode', and 'addEdge'
data Graph = Graph
  { _graphDeps :: S.Seq Dependency
  , _graphAssocs :: IM.IntMap IS.IntSet -- references dependencies by their position in the _graphDeps Seq
  , _graphDirect :: IS.IntSet
  }
  deriving (Eq, Ord, Show)

-- | Retrieve the nodes of the dependency graph
graphDeps :: Graph -> S.Seq Dependency
graphDeps = _graphDeps

-- | Retrieve the associations of the dependency graph
-- The returned 'IntMap' represents dependency relationships from Parent -> [Child]
graphAssocs :: Graph -> IM.IntMap IS.IntSet
graphAssocs = _graphAssocs

-- | Retrieve the associations of the dependency graph
-- The returned 'IntMap' represents dependency relationships from Parent -> [Child]
graphDirect :: Graph -> IS.IntSet
graphDirect = _graphDirect

-- | The empty graph
empty :: Graph
empty = Graph S.empty IM.empty IS.empty

-- | Add a new dependency node to the graph. The returned 'DepRef' can be used to 'addEdge's
addNode :: Dependency -> Graph -> (Graph, DepRef)
addNode dep graph = (graph{_graphDeps = curDeps S.|> dep}, DepRef (length curDeps))
  where
    curDeps = _graphDeps graph

-- | Add an edge to the dependency graph
addEdge :: DepRef -> DepRef -> Graph -> Graph
addEdge parent child graph = graph{_graphAssocs = IM.insertWith (<>) (unDepRef parent) (IS.singleton (unDepRef child)) (_graphAssocs graph)}

addDirect :: DepRef -> Graph -> Graph
addDirect dep graph = graph{_graphDirect = IS.insert (unDepRef dep) (_graphDirect graph)}

-- Graph is a semigroup: dependencies can be combined, with offsets applied to
-- the assocs and direct deps of the second graph
instance Semigroup Graph where
  Graph deps1 assocs1 direct1 <> Graph deps2 assocs2 direct2 =
    Graph
      (deps1 <> deps2) -- combine deps
      (IM.union assocs1 offsetAssocs2) -- offset the assocs entries
      (direct1 <> IS.map (+ offset) direct2) -- offset the direct entries
    where
      offsetAssocs2 =
        assocs2
          & IM.toList -- [(key, IntSet)]
          & map (bimap (+ offset) (IS.map (+ offset)))
          & IM.fromList
      offset = length deps1

instance Monoid Graph where
  mempty = empty

instance ToJSON Graph where
  toJSON Graph{..} =
    object
      [ "deps" .= _graphDeps
      , "assocs" .= _graphAssocs
      , "direct" .= _graphDirect
      ]
