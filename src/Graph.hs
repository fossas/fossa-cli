module Graph
  ( VerConstraint(..)
  , DepRef()
  , DepType(..)
  , Graph()
  , empty
  , addDirect
  , addEdge
  , addNode
  , graphAssocs
  , graphDeps
  , graphDirect

  , Dependency(..)
  ) where

import Prologue hiding (empty, parent)

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as S
import           Optics

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
addEdge parent child graph = graph { _graphAssocs = IM.insertWith (<>) (unDepRef parent) (IS.singleton (unDepRef child)) (_graphAssocs graph) }

addDirect :: DepRef -> Graph -> Graph
addDirect dep graph = graph { _graphDirect = IS.insert (unDepRef dep) (_graphDirect graph) }

data Dependency = Dependency
  { dependencyType      :: DepType
  , dependencyName      :: Text
  , dependencyVersion   :: Maybe VerConstraint
  , dependencyLocations :: [Text]
  , dependencyTags      :: Map Text [Text]
  } deriving (Eq, Ord, Show, Generic)

-- | A Dependency type. This corresponds to a "fetcher" on the backend
data DepType =
    NodeJSType -- ^ NPM registry (or similar)
  | PipType    -- ^ Pip registry
  deriving (Eq, Ord, Show, Generic)

data VerConstraint =
    CEq Text -- ^ equal to, e.g., @CEq "2.0.0"@
  | CURI Text -- ^ An exact version, at some URI
  | CCompatible Text -- ^ compatible range. e.g., "~=" in python, "^>=" in haskell
  | CAnd VerConstraint VerConstraint -- ^ Both constraints need to be satisfied, e.g., @CAnd (CGreaterOrEq "1.0.0") (CLessThan "2.0.0")@
  | COr  VerConstraint VerConstraint -- ^ At least one constraint needs to be satisfied
  | CLess Text -- ^ less than
  | CLessOrEq Text -- ^ less than or equal to
  | CGreater Text -- ^ greater than
  | CGreaterOrEq Text -- ^ greater than or equal to
  | CNot Text -- ^ not this version
  deriving (Eq, Ord, Show, Generic)

-- | Opaque reference to a dependency in the graph. Used for adding edges to the graph (See: 'addEdge')
newtype DepRef = DepRef { unDepRef :: Int } deriving (Eq, Ord, Show, Generic)

-- | A Graph of dependencies. See 'empty', 'addNode', and 'addEdge'
data Graph = Graph
  { _graphDeps   :: S.Seq Dependency
  , _graphAssocs :: IM.IntMap IS.IntSet -- references dependencies by their position in the _graphDeps Seq
  , _graphDirect :: IS.IntSet
  } deriving (Eq, Ord, Show, Generic)

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

-- Graph is a semigroup: dependencies can be combined, with offsets applied to
-- the assocs and direct deps of the second graph
instance Semigroup Graph where
  Graph deps1 assocs1 direct1 <> Graph deps2 assocs2 direct2 =
    Graph (deps1 <> deps2) -- combine deps
          (IM.union assocs1 offsetAssocs2) -- offset the assocs entries
          (direct1 <> IS.map (+offset) direct2) -- offset the direct entries
    where
    offsetAssocs2 = assocs2
                  & IM.toList -- [(key, IntSet)]
                  & over (mapped % _1) (+offset) -- offset keys
                  & over (mapped % _2) (IS.map (+offset)) -- offset set of values
                  & IM.fromList
    offset = length deps1

instance Monoid Graph where
  mempty = empty

instance FromJSON DepType -- use the generic instance
instance ToJSON DepType -- use the generic instance

instance FromJSON Dependency where
  parseJSON = withObject "Dependency" $ \obj ->
    Dependency <$> obj .: "type"
               <*> obj .: "name"
               <*> obj .: "version"
               <*> obj .: "locations"
               <*> obj .: "tags"

instance ToJSON Dependency where
  toJSON Dependency{..} = object
    [ "type"      .= dependencyType
    , "name"      .= dependencyName
    , "version"   .= dependencyVersion
    , "locations" .= dependencyLocations
    , "tags"      .= dependencyTags
    ]

instance FromJSON VerConstraint where
  parseJSON = undefined -- TODO

instance ToJSON VerConstraint where
  toJSON = undefined -- TODO

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
