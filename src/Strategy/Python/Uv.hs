module Strategy.Python.Uv (
  discover,
  findProjects,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  errCtx,
  errHelp,
  recover,
  run,
  warnOnErr,
 )
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (PipType),
  Dependency (..),
  VerConstraint (CEq),
  insertEnvironment,
  insertLocation,
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (..),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Grapher (
  Grapher,
  LabeledGrapher,
  deep,
  direct,
  edge,
  evalGrapher,
  label,
  withLabeling,
 )
import Effect.ReadFS (ReadFS, readContentsToml)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, parent)
import Toml.Schema qualified
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (PipenvProjectType, UvProjectType),
  GraphBreadth (Complete),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject UvProject]
discover = simpleDiscover findProjects mkProject PipenvProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [UvProject]
findProjects = walkWithFilters' $ \_ _ files -> do
  case findFileNamed "uv.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([UvProject file], WalkSkipAll)

newtype UvProject = UvProject
  {uvLockfile :: Path Abs File}
  deriving (Eq, Ord, Show, Generic)
instance ToJSON UvProject

mkProject :: UvProject -> DiscoveredProject UvProject
mkProject project =
  DiscoveredProject
    { projectType = UvProjectType
    , projectBuildTargets = mempty
    , projectPath = parent $ uvLockfile project
    , projectData = project
    }

instance AnalyzeProject UvProject where
  analyzeProject _ = getDepsStatically
  analyzeProjectStaticOnly _ = getDepsStatically

getDepsStatically ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  UvProject -> m DependencyResults
getDepsStatically project = context "uv" $ do
  lock <- context "Getting dependencies from uv.lock" $ readContentsToml (uvLockfile project)

  let graph = buildGraph lock
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [uvLockfile project]
      }

buildGraph :: UvLock -> Graphing Dependency
buildGraph lock = run . evalGrapher $ Map.traverseWithKey mkEdges nodes
  where
    nodes = findNodes lock

    mkEdges :: Has (Grapher Dependency) sig m => Text -> (Dependency, [Text]) -> m ()
    mkEdges _ (fromDep, transitiveDeps) =
      for_ transitiveDeps $ \name ->
        case Map.lookup name nodes of
          Nothing -> pure ()
          Just toDep -> edge fromDep $ fst toDep

findNodes :: UvLock -> Map Text (Dependency, [Text])
findNodes lock =
  Map.fromList $
    map (\p -> (uvlockPackageName p, toDepWithTransitiveDeps p)) $
      uvlockPackages lock
  where
    toDepWithTransitiveDeps pkg =
      ( Dependency
          { dependencyType = PipType
          , dependencyName = uvlockPackageName pkg
          , dependencyVersion = Just $ CEq $ uvlockPackageVersion pkg
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      , map uvlockPackageDependencyName $ uvlockPackageDependencies pkg
      )

---------- uv.lock

newtype UvLock = UvLock
  {uvlockPackages :: [UvLockPackage]}
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLock where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLock
        <$> Toml.Schema.reqKey "package"

data UvLockPackage = UvLockPackage
  { uvlockPackageName :: Text
  , uvlockPackageVersion :: Text
  , uvlockPackageSource :: UvLockPackageSource
  , uvlockPackageDependencies :: [UvLockPackageDependency]
  }
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLockPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackage
        <$> Toml.Schema.reqKey "name"
        <*> Toml.Schema.reqKey "version"
        <*> Toml.Schema.reqKey "source"
        <*> (fromMaybe [] <$> Toml.Schema.optKey "dependencies")

newtype UvLockPackageDependency = UvLockPackageDependency
  {uvlockPackageDependencyName :: Text}
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLockPackageDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageDependency
        <$> Toml.Schema.reqKey "name"

newtype UvLockPackageSource = UvLockPackageSource
  {uvLockPackageDependencyUrl :: Maybe Text}
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLockPackageSource where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageSource
        <$> Toml.Schema.optKey "url"
