module Graph
  ( DepRef()
  , DepType(..)
  , Graph()
  , empty
  , addDirect
  , addEdge
  , addNode
  , graphAssocs
  , graphDeps

  , Dependency(..)
  ) where

import Prologue hiding (empty, parent)

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as S

-- | The empty graph
empty :: Graph
empty = Graph S.empty IM.empty IS.empty

-- | Add a new dependency node to the graph. The returned 'DepRef' can be used to 'addEdge's
addNode :: Dependency -> Graph -> (DepRef, Graph)
addNode dep graph = (DepRef (length curDeps), graph { _graphDeps = curDeps S.|> dep })
  where
  curDeps = _graphDeps graph

-- | Add an edge to the dependency graph
addEdge :: DepRef -> DepRef -> Graph -> Graph
addEdge parent child graph = graph { _graphAssocs = IM.insertWith (++) (unDepRef parent) [unDepRef child] (_graphAssocs graph) }

addDirect :: DepRef -> Graph -> Graph
addDirect dep graph = graph { _graphDirect = IS.insert (unDepRef dep) (_graphDirect graph) }

data Dependency = Dependency
  { dependencyType      :: DepType
  , dependencyName      :: Text
  , dependencyVersion   :: Maybe Text -- TODO: constraints
  , dependencyLocations :: [Text]
  } deriving (Eq, Ord, Show, Generic)

-- | A Dependency type. This corresponds to a "fetcher" on the backend
data DepType =
  NodeJSType -- ^ NPM registry (or similar)
  deriving (Eq, Ord, Show, Generic)

-- | Opaque reference to a dependency in the graph. Used for adding edges to the graph (See: 'addEdge')
newtype DepRef = DepRef { unDepRef :: Int }

-- | A Graph of dependencies. See 'empty', 'addNode', and 'addEdge'
data Graph = Graph
  { _graphDeps   :: S.Seq Dependency
  , _graphAssocs :: IM.IntMap [Int] -- references dependencies by their position in the _graphDeps Seq
  , _graphDirect :: IS.IntSet
  } deriving (Eq, Ord, Show, Generic)

-- | Retrieve the nodes of the dependency graph
graphDeps :: Graph -> S.Seq Dependency
graphDeps = _graphDeps

-- | Retrieve the associations of the dependency graph
-- The returned 'IntMap' represents dependency relationships from Parent -> [Child]
graphAssocs :: Graph -> IM.IntMap [Int]
graphAssocs = _graphAssocs

instance FromJSON DepType -- use the generic instance
instance ToJSON DepType -- use the generic instance

instance FromJSON Dependency where
  parseJSON = withObject "Dependency" $ \obj ->
    Dependency <$> obj .: "type"
               <*> obj .: "name"
               <*> obj .: "version"
               <*> obj .: "locations"

instance ToJSON Dependency where
  toJSON Dependency{..} = object
    [ "type"      .= dependencyType
    , "name"      .= dependencyName
    , "version"   .= dependencyVersion
    , "locations" .= dependencyLocations
    ]

instance ToJSON Graph where
  toJSON Graph{..} = object
    [ "deps"   .= _graphDeps
    , "assocs" .= _graphAssocs
    , "direct" .= _graphDirect
    ]

instance FromJSON Graph where
  parseJSON = withObject "Graph" $ \obj ->
    Graph <$> obj .: "deps"
          <*> obj .: "assocs"
          <*> obj .: "direct"
