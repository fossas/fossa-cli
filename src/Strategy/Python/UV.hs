module Strategy.Python.UV (
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
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS, readContentsToml)
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
    Just file -> pure ([UvProject file], WalkContinue)

newtype UvProject = UvProject
  {uvLockfile :: Path Abs File}
  deriving (Eq, Ord, Show)

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
buildGraph lock = undefined

---------- uv.lock

newtype UvLock = UvLock
  {uvlockPackages :: [UvLockPackage]}
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLock where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLock
        <$> Toml.Schema.reqKey "packages"

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
        <*> Toml.Schema.reqKey "dependencies"

newtype UvLockPackageDependency = UvLockPackageDependency
  {uvLockPackageDependencyName :: Text}
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLockPackageDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageDependency
        <$> Toml.Schema.reqKey "name"

newtype UvLockPackageSource = UvLockPackageSource
  {uvLockPackageDependencyUrl :: Text}
  deriving (Eq, Show)

instance Toml.Schema.FromValue UvLockPackageSource where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageSource
        <$> Toml.Schema.reqKey "url"
