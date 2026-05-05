{-# LANGUAGE RecordWildCards #-}

module Strategy.Python.Uv (
  discover,
  findProjects,
  buildGraph,
  UvProject (..),
  UvLock (..),
  UvLockPackage (..),
  UvLockPackageSource (..),
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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, PipType, URLType, UnresolvedPathType),
  Dependency (..),
  VerConstraint (CEq),
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
  edge,
  evalGrapher,
 )
import Effect.ReadFS (ReadFS, readContentsToml)
import GHC.Generics (Generic)
import Graphing (
  Graphing,
  directList,
  getRootsOf,
  gmap,
  hasPredecessors,
  promoteToDirect,
  shrink,
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

discover ::
  (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) =>
  Path Abs Dir ->
  m [DiscoveredProject UvProject]
discover = simpleDiscover findProjects mkProject PipenvProjectType

findProjects ::
  (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) =>
  Path Abs Dir ->
  m [UvProject]
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
  analyzeProject _ = analyze
  analyzeProjectStaticOnly _ = analyze

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  UvProject ->
  m DependencyResults
analyze project = context "uv" $ do
  lock <- context "Getting dependencies from uv.lock" $ readContentsToml (uvLockfile project)

  let graph = buildGraph lock
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [uvLockfile project]
      }

buildGraph :: UvLock -> Graphing Dependency
buildGraph lock = removeWorkspacePackages . processGraph $ run . evalGrapher $ do
  traverse_ mkEdges packages
  where
    packages = uvlockPackages lock
    packagesByName = Map.fromList $ map (\p -> (uvlockPackageName p, p)) packages

    -- Workspace packages (editable/virtual) are the user's own code, not third-party deps.
    -- We include them during graph construction (for edges and env labeling) but remove them
    -- from the final output. shrink rewires edges through removed nodes to preserve transitivity.
    workspaceNames =
      Set.fromList
        [uvlockPackageName p | p <- uvlockPackages lock, isWorkspacePackage (uvlockPackageSource p)]
    removeWorkspacePackages = shrink (\dep -> not $ dependencyName dep `Set.member` workspaceNames)

    -- All nodes are added as deep dependencies. We will figure out direct dependencies later by
    -- calling `markDirectDeps` and then `shrinkRoots`.
    mkEdges :: Has (Grapher UvLockPackage) sig m => UvLockPackage -> m ()
    mkEdges fromPkg@UvLockPackage{..} = do
      for_ uvlockPackageDependencies $ \name ->
        for_ (Map.lookup name packagesByName) (edge fromPkg)
      for_ uvlockPackageDevDependencies $ \name ->
        for_ (Map.lookup name packagesByName) (edge fromPkg)
      for_ uvlockPackageOptionalDependencies $ traverse_ $ \name ->
        for_ (Map.lookup name packagesByName) (edge fromPkg)

    -- The directList currently contains the uv package being scanned. This package lists the
    -- prod dependencies (under the dependencies field) and the dev dependencies (under the dev-dependencies field)
    -- Use this to set the correct environment on each of these packages.
    markRootEnvs :: Graphing UvLockPackage -> Graphing Dependency
    markRootEnvs gr = gmap (applyLabels) gr
      where
        applyLabels :: UvLockPackage -> Dependency
        applyLabels pkg = toDependency pkg $ newEnvs pkg

        newEnvs :: UvLockPackage -> Set DepEnvironment
        newEnvs UvLockPackage{..} =
          Set.fromList $
            catMaybes
              [ maybeElem prodDeps EnvProduction uvlockPackageName
              , maybeElem devDeps EnvDevelopment uvlockPackageName
              ]

        maybeElem :: (Eq a) => [a] -> b -> a -> Maybe b
        maybeElem list def toFind = if toFind `elem` list then Just def else Nothing

        prodDeps = foldMap uvlockPackageDependencies $ directList gr
        -- Legacy format has dev dependencies under the dev-dependencies field
        -- New format has dev dependencies under optional-dependencies.dev
        devDeps =
          foldMap uvlockPackageDevDependencies (directList gr)
            <> foldMap (fromMaybe [] . Map.lookup "dev" . uvlockPackageOptionalDependencies) (directList gr)

    -- Locate the root nodes of the graph and mark these as direct dependencies
    -- The root node will be the uv package being scanned, not its dependencies, so we will need to later
    -- call `shrinkRoots` to remove this package and promote its dependencies to direct dependencies
    markDirectDeps :: Graphing UvLockPackage -> Graphing UvLockPackage
    markDirectDeps gr = promoteToDirect (not . hasPredecessors gr) gr

    toDependency :: UvLockPackage -> Set DepEnvironment -> Dependency
    toDependency UvLockPackage{..} envs =
      case uvlockPackageSource of
        SourceGit url ->
          Dependency
            { dependencyType = GitType
            , dependencyName = gitBaseUrl url
            , dependencyVersion = CEq <$> gitCommitHash url
            , dependencyLocations = []
            , dependencyEnvironments = envs
            , dependencyTags = Map.empty
            }
        SourcePath path ->
          Dependency
            { dependencyType = UnresolvedPathType
            , dependencyName = path
            , dependencyVersion = CEq <$> uvlockPackageVersion
            , dependencyLocations = []
            , dependencyEnvironments = envs
            , dependencyTags = Map.empty
            }
        SourceDirectory path ->
          Dependency
            { dependencyType = UnresolvedPathType
            , dependencyName = path
            , dependencyVersion = CEq <$> uvlockPackageVersion
            , dependencyLocations = []
            , dependencyEnvironments = envs
            , dependencyTags = Map.empty
            }
        SourceUrl url ->
          Dependency
            { dependencyType = URLType
            , dependencyName = url
            , dependencyVersion = CEq <$> uvlockPackageVersion
            , dependencyLocations = []
            , dependencyEnvironments = envs
            , dependencyTags = Map.empty
            }
        _ ->
          Dependency
            { dependencyType = PipType
            , dependencyName = uvlockPackageName
            , dependencyVersion = CEq <$> uvlockPackageVersion
            , dependencyLocations = []
            , dependencyEnvironments = envs
            , dependencyTags = Map.empty
            }

    -- Git URLs in uv.lock: "https://github.com/owner/repo?tag=v1.0#abc123"
    -- Base URL is everything before the query/fragment
    gitBaseUrl :: Text -> Text
    gitBaseUrl = fst . Text.breakOn "?"

    -- Commit hash is the fragment after '#'
    gitCommitHash :: Text -> Maybe Text
    gitCommitHash url =
      case Text.splitOn "#" url of
        [_, hash] | not (Text.null hash) -> Just hash
        _ -> Nothing

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
        rootEnvs dep =
          foldMap dependencyEnvironments (getRootsOf gr dep)
            <> ( if dep `elem` directList gr
                   then dependencyEnvironments dep
                   else Set.empty
               )

    -- In order to label environments correctly with only the uv.lock file, we need to build the graph
    -- first so that we have edges and can traverse the graph. Once we do that, we have enough information
    -- to label all the packages with the correct environment
    processGraph = markDevDeps . shrinkRoots . markRootEnvs . markDirectDeps

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
  , uvlockPackageVersion :: Maybe Text
  , uvlockPackageSource :: UvLockPackageSource
  , uvlockPackageDependencies :: [Text]
  , uvlockPackageDevDependencies :: [Text]
  , uvlockPackageOptionalDependencies :: Map Text [Text]
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackage
        <$> Toml.Schema.reqKey "name"
        <*> Toml.Schema.optKey "version"
        <*> Toml.Schema.reqKey "source"
        <*> (maybe [] (map uvlockPackageDependencyName) <$> Toml.Schema.optKey "dependencies")
        <*> (maybe [] uvlockPackageDevDependenciesInt <$> Toml.Schema.optKey "dev-dependencies")
        <*> Toml.Schema.pickKey
          [ Toml.Schema.Key
              "optional-dependencies"
              (fmap (Map.map (map uvlockPackageDependencyName)) . Toml.Schema.fromValue)
          , Toml.Schema.Else (pure mempty)
          ]

newtype UvLockPackageDependency = UvLockPackageDependency
  {uvlockPackageDependencyName :: Text}
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackageDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageDependency
        <$> Toml.Schema.reqKey "name"

data UvLockPackageSource
  = SourceEditable Text
  | SourceVirtual Text
  | SourceRegistry Text
  | SourceGit Text
  | SourceUrl Text
  | SourcePath Text
  | SourceDirectory Text
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackageSource where
  fromValue =
    Toml.Schema.parseTableFromValue $
      Toml.Schema.pickKey
        [ Toml.Schema.Key "editable" (fmap SourceEditable . Toml.Schema.fromValue)
        , Toml.Schema.Key "virtual" (fmap SourceVirtual . Toml.Schema.fromValue)
        , Toml.Schema.Key "registry" (fmap SourceRegistry . Toml.Schema.fromValue)
        , Toml.Schema.Key "git" (fmap SourceGit . Toml.Schema.fromValue)
        , Toml.Schema.Key "url" (fmap SourceUrl . Toml.Schema.fromValue)
        , Toml.Schema.Key "path" (fmap SourcePath . Toml.Schema.fromValue)
        , Toml.Schema.Key "directory" (fmap SourceDirectory . Toml.Schema.fromValue)
        ]

isWorkspacePackage :: UvLockPackageSource -> Bool
isWorkspacePackage (SourceEditable _) = True
isWorkspacePackage (SourceVirtual _) = True
isWorkspacePackage _ = False

newtype UvLockPackageDevDependencies = UvLockPackageDevDependencies
  {uvlockPackageDevDependenciesInt :: [Text]}
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackageDevDependencies where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageDevDependencies . maybe [] (map uvlockPackageDependencyName) <$> Toml.Schema.optKey "dev"
