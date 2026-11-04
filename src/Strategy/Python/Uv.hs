{-# LANGUAGE RecordWildCards #-}

module Strategy.Python.Uv (
  discover,
  findProjects,
  buildGraph,
  UvProject (..),
  UvLock (..),
  UvLockPackage (..),
  UvLockPackageDependency (..),
  UvLockPackageSource (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import App.Fossa.Config.Analyze (StrategyConfig (uvConfig))
import App.Fossa.Config.ConfigFile (UvConfig (..))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  run,
 )
import Control.Effect.Reader (Reader, asks)
import Data.Aeson (ToJSON)
import Data.Foldable (fold, for_, traverse_)
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
  induce,
  promoteToDirect,
  pruneUnreachable,
  shrink,
  shrinkRoots,
  vertexList,
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
  , Has (Reader StrategyConfig) sig m
  ) =>
  UvProject ->
  m DependencyResults
analyze project = context "uv" $ do
  cfg <- asks uvConfig
  lock <- context "Getting dependencies from uv.lock" $ readContentsToml (uvLockfile project)

  let graph = buildGraph cfg lock
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [uvLockfile project]
      }

buildGraph :: Maybe UvConfig -> UvLock -> Graphing Dependency
buildGraph cfg lock = removeWorkspacePackages . processGraph $ graphOf excludedGroups
  where
    packages = uvlockPackages lock
    packagesByName = Map.fromListWith (<>) $ map (\p -> (uvlockPackageName p, [p])) packages

    resolveDependency :: UvLockPackageDependency -> [UvLockPackage]
    resolveDependency dep =
      filter (dependencyMatches dep) $ Map.findWithDefault [] (uvlockPackageDependencyName dep) packagesByName

    -- Workspace packages (editable/virtual) are the user's own code, not third-party deps.
    -- We include them during graph construction (for edges and env labeling) but remove them
    -- from the final output. shrink rewires edges through removed nodes to preserve transitivity.
    workspaceNames =
      Set.fromList
        [uvlockPackageName p | p <- uvlockPackages lock, isWorkspacePackage (uvlockPackageSource p)]
    removeWorkspacePackages = shrink (\dep -> not $ dependencyName dep `Set.member` workspaceNames)

    productionGroups = maybe mempty (Set.map normalizeGroupName . uvProductionGroups) cfg
    excludedGroups = maybe mempty (Set.map normalizeGroupName . uvExcludeGroups) cfg

    -- A package's dependency groups, by name, without the excluded ones.
    groups :: UvLockPackage -> Map Text [UvLockPackageDependency]
    groups pkg = Map.withoutKeys (uvlockPackageDevDependencies pkg) excludedGroups

    -- Every package's edges, leaving out the dependency groups named in `excluded`.
    graphOf :: Set Text -> Graphing UvLockPackage
    graphOf excluded = run . evalGrapher $ traverse_ (mkEdges excluded) packages

    -- All nodes are added as deep dependencies. We will figure out direct dependencies later by
    -- calling `markDirectDeps` and then `shrinkRoots`.
    -- A group or extra can install the package itself, e.g. `dev = ["my-project[test]"]`. That
    -- edge is skipped, since it would leave the project with a predecessor and no longer a root.
    mkEdges :: Has (Grapher UvLockPackage) sig m => Set Text -> UvLockPackage -> m ()
    mkEdges excluded fromPkg@UvLockPackage{..} =
      for_ (uvlockPackageDependencies <> fold (Map.withoutKeys uvlockPackageDevDependencies excluded) <> fold uvlockPackageOptionalDependencies) $
        traverse_ (edge fromPkg) . filter (/= fromPkg) . resolveDependency

    -- The directList currently contains the uv package being scanned. This package lists the
    -- prod dependencies (under the dependencies field) and its dependency groups (under the dev-dependencies field)
    -- Use this to set the correct environment on each of these packages.
    markRootEnvs :: Graphing UvLockPackage -> Graphing Dependency
    markRootEnvs gr = gmap (applyLabels) gr
      where
        applyLabels :: UvLockPackage -> Dependency
        applyLabels pkg = toDependency pkg $ newEnvs pkg

        newEnvs :: UvLockPackage -> Set DepEnvironment
        newEnvs pkg =
          Set.fromList $
            catMaybes
              [ referencedBy prodDeps EnvProduction
              , referencedBy devDeps EnvDevelopment
              ]
          where
            referencedBy :: [UvLockPackageDependency] -> DepEnvironment -> Maybe DepEnvironment
            referencedBy deps env = if any (`dependencyMatches` pkg) deps then Just env else Nothing

        prodDeps =
          foldMap uvlockPackageDependencies (directList gr)
            <> foldMap (fold . (`Map.restrictKeys` productionGroups) . groups) (directList gr)
        -- Every other dependency group is dev. Older projects list dev dependencies as an extra,
        -- under optional-dependencies.dev.
        devDeps =
          foldMap (fold . (`Map.withoutKeys` productionGroups) . groups) (directList gr)
            <> foldMap (fromMaybe [] . Map.lookup "dev" . uvlockPackageOptionalDependencies) (directList gr)

    -- Locate the root nodes of the graph and mark these as direct dependencies
    -- The root node will be the uv package being scanned, not its dependencies, so we will need to later
    -- call `shrinkRoots` to remove this package and promote its dependencies to direct dependencies
    -- Roots are found with every group's edges drawn, since an excluded group's packages lose their edge
    -- from the project but aren't roots. Packages that only an excluded group reaches are dropped.
    markDirectDeps :: Graphing UvLockPackage -> Graphing UvLockPackage
    markDirectDeps gr = induce (`Set.notMember` excludedOnly) (promoteToDirect isRoot gr)
      where
        withEveryGroup = graphOf mempty
        isRoot = not . hasPredecessors withEveryGroup
        reachable = Set.fromList . vertexList . pruneUnreachable . promoteToDirect isRoot
        excludedOnly = reachable withEveryGroup `Set.difference` reachable gr

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
  , uvlockPackageDependencies :: [UvLockPackageDependency]
  , uvlockPackageDevDependencies :: Map Text [UvLockPackageDependency]
  -- ^ Every dependency group (https://peps.python.org/pep-0735/) by name, from `[package.dev-dependencies]`
  , uvlockPackageOptionalDependencies :: Map Text [UvLockPackageDependency]
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackage
        <$> Toml.Schema.reqKey "name"
        <*> Toml.Schema.optKey "version"
        <*> Toml.Schema.reqKey "source"
        <*> (fromMaybe [] <$> Toml.Schema.optKey "dependencies")
        <*> (fromMaybe mempty <$> Toml.Schema.optKey "dev-dependencies")
        <*> Toml.Schema.pickKey
          [ Toml.Schema.Key
              "optional-dependencies"
              Toml.Schema.fromValue
          , Toml.Schema.Else (pure mempty)
          ]

data UvLockPackageDependency = UvLockPackageDependency
  { uvlockPackageDependencyName :: Text
  , uvlockPackageDependencyVersion :: Maybe Text
  , uvlockPackageDependencySource :: Maybe UvLockPackageSource
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue UvLockPackageDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      UvLockPackageDependency
        <$> Toml.Schema.reqKey "name"
        <*> Toml.Schema.optKey "version"
        <*> Toml.Schema.optKey "source"

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

-- uv only records a dependency's version and source when the lockfile contains more than one
-- package with that name (e.g. different versions resolved for different Python versions), so
-- they are only compared when present.
dependencyMatches :: UvLockPackageDependency -> UvLockPackage -> Bool
dependencyMatches UvLockPackageDependency{..} UvLockPackage{..} =
  uvlockPackageDependencyName == uvlockPackageName
    && all (\v -> uvlockPackageVersion == Just v) uvlockPackageDependencyVersion
    && all (== uvlockPackageSource) uvlockPackageDependencySource

isWorkspacePackage :: UvLockPackageSource -> Bool
isWorkspacePackage (SourceEditable _) = True
isWorkspacePackage (SourceVirtual _) = True
isWorkspacePackage _ = False

-- uv writes dependency group names to uv.lock normalized as PEP 503 describes: lowercased, with each
-- run of '-', '_', and '.' replaced by a single '-'. https://peps.python.org/pep-0503/#normalized-names
normalizeGroupName :: Text -> Text
normalizeGroupName = Text.toLower . foldMap collapse . Text.groupBy (\a b -> isSeparator a == isSeparator b)
  where
    isSeparator = (`elem` ['-', '_', '.'])
    collapse chunk = if Text.any isSeparator chunk then "-" else chunk
