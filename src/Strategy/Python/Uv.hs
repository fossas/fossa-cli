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
import Data.Foldable (for_, traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
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
import Graphing (Graphing, preSet, promoteToDirect, shrinkRoots)
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
    Just file -> pure ([UvProject file], WalkSkipSome [".venv"])

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
buildGraph lock = shrinkRoots $ markDirectDeps $ run . evalGrapher $ do
  traverse_ mkEdges $ uvlockPackages lock
  where
    packagesByName = Map.fromList $ map (\p -> (uvlockPackageName p, p)) $ uvlockPackages lock

    mkEdges :: Has (Grapher Dependency) sig m => UvLockPackage -> m ()
    mkEdges fromPkg@UvLockPackage{uvlockPackageDependencies} =
      for_ uvlockPackageDependencies $ \name ->
        case Map.lookup name packagesByName of
          Nothing -> pure ()
          Just toPkg -> edge (toDependency fromPkg) (toDependency toPkg)

    markDirectDeps :: Graphing Dependency -> Graphing Dependency
    markDirectDeps gr = promoteToDirect (`isRootNode` gr) gr

    isRootNode :: Dependency -> Graphing Dependency -> Bool
    isRootNode dep gr = Set.null $ preSet dep gr

    toDependency :: UvLockPackage -> Dependency
    toDependency pkg =
      Dependency
        { dependencyType = PipType
        , dependencyName = uvlockPackageName pkg
        , dependencyVersion = Just $ CEq $ uvlockPackageVersion pkg
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

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
  , uvlockPackageDependencies :: [Text]
  }
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLockPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackage
        <$> Toml.Schema.reqKey "name"
        <*> Toml.Schema.reqKey "version"
        <*> Toml.Schema.reqKey "source"
        <*> (maybe [] (map uvlockPackageDependencyName) <$> Toml.Schema.optKey "dependencies")

newtype UvLockPackageDependency = UvLockPackageDependency
  {uvlockPackageDependencyName :: Text}
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLockPackageDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageDependency
        <$> Toml.Schema.reqKey "name"

newtype UvLockPackageSource = UvLockPackageSource
  {uvlockPackageDependencyUrl :: Maybe Text}
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLockPackageSource where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageSource
        <$> Toml.Schema.optKey "url"
