module Strategy.Bazel.MavenInstall (
  MavenInstallJson (..),
  MavenArtifactInfo (..),
  parseMavenInstall,
  buildMavenInstallGraph,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.:),
  (.:?),
 )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Graphing (Graphing)
import Graphing qualified

-- | Parsed maven_install.json lockfile.
data MavenInstallJson = MavenInstallJson
  { mavenArtifacts :: Map Text MavenArtifactInfo
  , mavenDependencyTree :: Maybe MavenDependencyTree
  }
  deriving (Eq, Ord, Show)

-- | Info about a single Maven artifact.
data MavenArtifactInfo = MavenArtifactInfo
  { artifactVersion :: Text
  }
  deriving (Eq, Ord, Show)

-- | The dependency_tree section of maven_install.json (v2 format).
data MavenDependencyTree = MavenDependencyTree
  { treeArtifacts :: [TreeArtifact]
  }
  deriving (Eq, Ord, Show)

-- | A single artifact entry in the dependency tree.
data TreeArtifact = TreeArtifact
  { treeCoord :: Text
  , treeDeps :: [Text]
  }
  deriving (Eq, Ord, Show)

instance FromJSON MavenInstallJson where
  parseJSON = withObject "MavenInstallJson" $ \obj -> do
    -- Try v2 format with dependency_tree first
    mDepTree <- obj .:? "dependency_tree"
    -- Try v1 format with artifacts map
    mArtifacts <- obj .:? "artifacts"
    pure
      MavenInstallJson
        { mavenArtifacts = fromMaybe Map.empty mArtifacts
        , mavenDependencyTree = mDepTree
        }

instance FromJSON MavenArtifactInfo where
  parseJSON = withObject "MavenArtifactInfo" $ \obj ->
    MavenArtifactInfo <$> obj .: "version"

instance FromJSON MavenDependencyTree where
  parseJSON = withObject "MavenDependencyTree" $ \obj ->
    MavenDependencyTree <$> obj .: "artifacts"

instance FromJSON TreeArtifact where
  parseJSON = withObject "TreeArtifact" $ \obj ->
    TreeArtifact
      <$> obj .: "coord"
      <*> obj .: "dependencies"

-- | Parse maven_install.json contents (provided externally via readContentsJson).
parseMavenInstall :: MavenInstallJson -> Graphing Dependency
parseMavenInstall = buildMavenInstallGraph

-- | Build a dependency graph from maven_install.json.
buildMavenInstallGraph :: MavenInstallJson -> Graphing Dependency
buildMavenInstallGraph installJson =
  case mavenDependencyTree installJson of
    Just tree -> buildFromTree tree
    Nothing -> buildFromArtifacts (mavenArtifacts installJson)

-- | Build graph from v2 dependency_tree format (has transitive deps).
buildFromTree :: MavenDependencyTree -> Graphing Dependency
buildFromTree tree =
  let artifacts = treeArtifacts tree
      -- All coords that appear as dependencies of something else
      childCoords = Set.fromList $ concatMap treeDeps artifacts
      -- Roots are artifacts that are NOT children of anything
      roots = filter (\a -> not (Set.member (treeCoord a) childCoords)) artifacts
      -- Build edges
      edgeGraph = mconcat [buildTreeEdges a | a <- artifacts]
      -- Mark roots as direct
      directGraph = Graphing.directs (map (coordToDep . treeCoord) roots)
   in directGraph <> edgeGraph

buildTreeEdges :: TreeArtifact -> Graphing Dependency
buildTreeEdges artifact =
  let parent = coordToDep (treeCoord artifact)
      children = map coordToDep (treeDeps artifact)
      edgeGraphs = map (Graphing.edge parent) children
   in mconcat edgeGraphs

-- | Build graph from v1 artifacts map (no transitive info).
buildFromArtifacts :: Map Text MavenArtifactInfo -> Graphing Dependency
buildFromArtifacts artifacts =
  Graphing.directs (map toArtifactDep (Map.toList artifacts))
  where
    toArtifactDep :: (Text, MavenArtifactInfo) -> Dependency
    toArtifactDep (coordKey, info) =
      Dependency
        { dependencyType = MavenType
        , dependencyName = coordKey
        , dependencyVersion = Just (CEq (artifactVersion info))
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

-- | Parse a Maven coordinate string like "group:artifact:version" or
-- "group:artifact:packaging:version" into a Dependency.
coordToDep :: Text -> Dependency
coordToDep coord =
  let parts = Text.splitOn ":" coord
      (name, version) = case parts of
        -- group:artifact:version
        [g, a, v] -> (g <> ":" <> a, Just (CEq v))
        -- group:artifact:packaging:version
        [g, a, _p, v] -> (g <> ":" <> a, Just (CEq v))
        -- group:artifact:packaging:classifier:version
        [g, a, _p, _c, v] -> (g <> ":" <> a, Just (CEq v))
        -- group:artifact (no version)
        [g, a] -> (g <> ":" <> a, Nothing)
        -- Fallback: use the whole string as name
        _ -> (coord, Nothing)
   in Dependency
        { dependencyType = MavenType
        , dependencyName = name
        , dependencyVersion = version
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
