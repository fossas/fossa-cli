{-# LANGUAGE RecordWildCards #-}

module Strategy.Python.Uv (
  discover,
  findProjects,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  run,
 )
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Foldable (for_, traverse_)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
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
  LabeledGrapher,
  edge,
  label,
  withLabeling,
 )
import Effect.ReadFS (ReadFS, readContentsToml)
import GHC.Generics (Generic)
import Graphing (
  Graphing,
  getRootsOf,
  gmap,
  hasPredecessors,
  promoteToDirect,
  shrinkRoots,
 )
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

data UvLabel = UvSource Text | UvEnvironment DepEnvironment deriving (Eq, Ord, Show)
type UvGrapher = LabeledGrapher UvLockPackage UvLabel

buildGraph :: UvLock -> Graphing Dependency
buildGraph lock = markDevDeps $ shrinkRoots $ markDirectDeps $ run . withLabeling toDependency $ do
  traverse_ mkEdges $ uvlockPackages lock
  where
    packagesByName = Map.fromList $ map (\p -> (uvlockPackageName p, p)) $ uvlockPackages lock

    mkEdges :: Has UvGrapher sig m => UvLockPackage -> m ()
    mkEdges fromPkg@UvLockPackage{..} = do
      for_ uvlockPackageDependencies $ \name ->
        case Map.lookup name packagesByName of
          Nothing -> pure ()
          Just toPkg -> edge fromPkg toPkg >> label toPkg (UvEnvironment EnvProduction)
      for_ uvlockPackageDevDependencies $ \name ->
        case Map.lookup name packagesByName of
          Nothing -> pure ()
          Just toPkg -> edge fromPkg toPkg >> label toPkg (UvEnvironment EnvDevelopment)

    markDirectDeps :: Graphing Dependency -> Graphing Dependency
    markDirectDeps gr = promoteToDirect (not . hasPredecessors gr) gr

    toDependency :: UvLockPackage -> Set UvLabel -> Dependency
    toDependency pkg = foldr applyLabel start
      where
        applyLabel (UvSource loc) = insertLocation loc
        applyLabel (UvEnvironment env) = insertEnvironment env
        start =
          Dependency
            { dependencyType = PipType
            , dependencyName = uvlockPackageName pkg
            , dependencyVersion = Just $ CEq $ uvlockPackageVersion pkg
            , dependencyLocations = []
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }

    -- We've labeled direct dependencies with the correct environment, but we need to
    -- propagate this environment to all the transitive dependencies. This will make it so that if
    -- package X is a dev dependency, then all the dependencies of X will also be labeled as dev
    -- dependencies.
    markDevDeps :: Graphing Dependency -> Graphing Dependency
    markDevDeps gr = gmap setEnvFromRoot gr
      where
        setEnvFromRoot :: Dependency -> Dependency
        setEnvFromRoot dep = dep{dependencyEnvironments = rootEnvs dep}

        rootEnvs :: Dependency -> Set DepEnvironment
        rootEnvs dep
          | not $ hasPredecessors gr dep = dependencyEnvironments dep
          | otherwise = foldMap dependencyEnvironments $ getRootsOf gr dep

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
  , uvlockPackageDevDependencies :: [Text]
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackage
        <$> Toml.Schema.reqKey "name"
        <*> Toml.Schema.reqKey "version"
        <*> Toml.Schema.reqKey "source"
        <*> (maybe [] (map uvlockPackageDependencyName) <$> Toml.Schema.optKey "dependencies")
        <*> (maybe [] uvlockPackageDevDependenciesInt <$> Toml.Schema.optKey "dev-dependencies")

newtype UvLockPackageDependency = UvLockPackageDependency
  {uvlockPackageDependencyName :: Text}
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackageDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageDependency
        <$> Toml.Schema.reqKey "name"

newtype UvLockPackageSource = UvLockPackageSource
  {uvlockPackageDependencyUrl :: Maybe Text}
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackageSource where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageSource
        <$> Toml.Schema.optKey "url"

newtype UvLockPackageDevDependencies = UvLockPackageDevDependencies
  {uvlockPackageDevDependenciesInt :: [Text]}
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackageDevDependencies where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageDevDependencies . maybe [] (map uvlockPackageDependencyName) <$> Toml.Schema.optKey "dev"
