module Strategy.Python.Poetry (
  discover,

  -- * for testing only
  graphFromLockFile,
  setGraphDirectsFromPyproject,
  PoetryProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context, errCtx, fatalText, recover, warnOnErr)
import Control.Effect.Reader (Reader)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import DepTypes (DepType (..), Dependency (..))
import Diag.Common (
  MissingDeepDeps (MissingDeepDeps),
  MissingEdges (MissingEdges),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Logger (Logger, Pretty (pretty), logDebug)
import Effect.ReadFS (ReadFS, readContentsToml)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, File, Path)
import Strategy.Python.Errors (
  MissingPoetryLockFile (MissingPoetryLockFile),
 )
import Strategy.Python.Poetry.Common (getPoetryBuildBackend, logIgnoredDeps, pyProjectDeps, toCanonicalName, toMap)
import Strategy.Python.Poetry.PoetryLock (PackageName (..), PoetryLock (..), PoetryLockPackage (..), poetryLockCodec)
import Strategy.Python.Poetry.PyProject (PyProject (..), pyProjectCodec)
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (PoetryProjectType), GraphBreadth (..))

newtype PyProjectTomlFile = PyProjectTomlFile {pyProjectTomlPath :: Path Abs File} deriving (Eq, Ord, Show, Generic)
newtype PoetryLockFile = PoetryLockFile {poetryLockPath :: Path Abs File} deriving (Eq, Ord, Show, Generic)
newtype ProjectDir = ProjectDir {pyProjectPath :: Path Abs Dir} deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectTomlFile
instance ToJSON PoetryLockFile
instance ToJSON ProjectDir

data PoetryProject = PoetryProject
  { projectDir :: ProjectDir
  , pyProjectToml :: PyProjectTomlFile
  , poetryLock :: Maybe PoetryLockFile
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON PoetryProject

instance AnalyzeProject PoetryProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject PoetryProject]
discover = simpleDiscover findProjects mkProject PoetryProjectType

-- | Poetry build backend identifier required in [pyproject.toml](https://python-poetry.org/docs/pyproject/#poetry-and-pep-517).
usesPoetryBackend :: Text -> Bool
usesPoetryBackend backend =
  backend == "poetry.core.masonry.api" -- For poetry versions >=1.1.0a1 (released 2020)
    || backend == "poetry.masonry.api" -- Refer to https://github.com/python-poetry/poetry/pull/2212

-- | Reference message text for poetry build backend setting value required in pyproject.toml.
-- Users should configure poetry build backend in pyproject.toml for poetry project discovery.
poetryBuildBackendIdentifierHelpText :: Text
poetryBuildBackendIdentifierHelpText = "Poetry project must use poetry build backend. Please refer to https://python-poetry.org/docs/pyproject/#poetry-and-pep-517."

warnIncorrectBuildBackend :: Has Logger sig m => Text -> m ()
warnIncorrectBuildBackend currentBackend =
  (logDebug . pretty) $
    "pyproject.toml does not use poetry build backend. It uses: "
      <> currentBackend
      <> "\n"
      <> poetryBuildBackendIdentifierHelpText

-- | Finds poetry project by searching for pyproject.toml.
-- If poetry.lock file is also discovered, it is used as a supplement.
findProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [PoetryProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let poetryLockFile = findFileNamed "poetry.lock" files
  let pyprojectFile = findFileNamed "pyproject.toml" files

  case (poetryLockFile, pyprojectFile) of
    (poetry, Just pyproject) -> do
      poetryProject <- readContentsToml pyProjectCodec pyproject
      let project = PoetryProject (ProjectDir dir) (PyProjectTomlFile pyproject) (PoetryLockFile <$> poetry)
      let pyprojectBuildBackend = getPoetryBuildBackend poetryProject

      case pyprojectBuildBackend of
        Nothing -> pure ([], WalkContinue)
        Just pbs ->
          if usesPoetryBackend pbs
            then pure ([project], WalkSkipAll)
            else ([], WalkContinue) <$ warnIncorrectBuildBackend pbs

    -- Without pyproject file, it is unlikely that project is a poetry project. Poetry itself does not work
    -- without [pyproject.toml manifest](https://python-poetry.org/docs/pyproject/).
    (Just _, Nothing) -> context "poetry.lock file found without accompanying pyproject.toml!" $ pure ([], WalkContinue)
    (Nothing, Nothing) -> pure ([], WalkContinue)

mkProject :: PoetryProject -> DiscoveredProject PoetryProject
mkProject project =
  DiscoveredProject
    { projectType = PoetryProjectType
    , projectBuildTargets = mempty
    , projectPath = pyProjectPath $ projectDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => PoetryProject -> m DependencyResults
getDeps project = do
  context "Poetry" $ context "Static analysis" $ analyze project

-- | Analyzes Poetry Project and creates dependency graph.
analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  PoetryProject ->
  m DependencyResults
analyze PoetryProject{pyProjectToml, poetryLock} = do
  pyproject <- readContentsToml pyProjectCodec (pyProjectTomlPath pyProjectToml)
  case poetryLock of
    Just lockPath -> do
      poetryLockProject <- readContentsToml poetryLockCodec (poetryLockPath lockPath)
      _ <- logIgnoredDeps pyproject (Just poetryLockProject)
      graph <- context "Building dependency graph from pyproject.toml and poetry.lock" $ pure $ setGraphDirectsFromPyproject (graphFromLockFile poetryLockProject) pyproject
      pure $
        DependencyResults
          { dependencyGraph = graph
          , dependencyGraphBreadth = Complete
          , dependencyManifestFiles = [poetryLockPath lockPath]
          }
    Nothing -> do
      void
        . recover
        . warnOnErr MissingDeepDeps
        . warnOnErr MissingEdges
        . errCtx (MissingPoetryLockFile (pyProjectTomlPath pyProjectToml))
        $ fatalText "poetry.lock file was not discovered"
      graph <- context "Building dependency graph from only pyproject.toml" $ pure $ Graphing.fromList $ pyProjectDeps pyproject
      pure $
        DependencyResults
          { dependencyGraph = graph
          , dependencyGraphBreadth = Partial
          , dependencyManifestFiles = [pyProjectTomlPath pyProjectToml]
          }

-- | Use a `pyproject.toml` to set the direct dependencies of a graph created from `poetry.lock`.
setGraphDirectsFromPyproject :: Graphing Dependency -> PyProject -> Graphing Dependency
setGraphDirectsFromPyproject graph pyproject = Graphing.promoteToDirect isDirect graph
  where
    -- Dependencies in `poetry.lock` are direct if they're specified in `pyproject.toml`.
    -- `pyproject.toml` may use non canonical naming, when naming dependencies.
    isDirect :: Dependency -> Bool
    isDirect dep = case pyprojectPoetry pyproject of
      Nothing -> False
      Just _ -> any (\n -> toCanonicalName (dependencyName n) == toCanonicalName (dependencyName dep)) $ pyProjectDeps pyproject

-- | Using a Poetry lockfile, build the graph of packages.
-- The resulting graph contains edges, but does not distinguish between direct and deep dependencies,
-- since `poetry.lock` does not indicate which dependencies are direct.
graphFromLockFile :: PoetryLock -> Graphing Dependency
graphFromLockFile poetryLock = Graphing.gmap pkgNameToDependency (edges <> Graphing.deeps pkgsNoDeps)
  where
    pkgs :: [PoetryLockPackage]
    pkgs = poetryLockPackages poetryLock

    pkgsNoDeps :: [PackageName]
    pkgsNoDeps = poetryLockPackageName <$> filter (null . poetryLockPackageDependencies) pkgs

    depsWithEdges :: [PoetryLockPackage]
    depsWithEdges = filter (not . null . poetryLockPackageDependencies) pkgs

    edgeOf :: PoetryLockPackage -> [(PackageName, PackageName)]
    edgeOf p = map tuplify . Map.keys $ poetryLockPackageDependencies p
      where
        tuplify :: Text -> (PackageName, PackageName)
        tuplify x = (poetryLockPackageName p, PackageName x)

    edges :: Graphing PackageName
    edges = Graphing.edges (concatMap edgeOf depsWithEdges)

    canonicalPkgName :: PackageName -> PackageName
    canonicalPkgName name = PackageName . toCanonicalName $ unPackageName name

    mapOfDependency :: Map PackageName Dependency
    mapOfDependency = toMap pkgs

    -- Pip packages are [case insensitive](https://www.python.org/dev/peps/pep-0508/#id21), but poetry.lock may use
    -- non-canonical name for reference. Try to lookup with provided name, otherwise fallback to canonical naming.
    pkgNameToDependency :: PackageName -> Dependency
    pkgNameToDependency name =
      fromMaybe
        ( Dependency
            { dependencyType = PipType
            , dependencyName = unPackageName name
            , dependencyVersion = Nothing
            , dependencyLocations = []
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }
        )
        $ Map.lookup name mapOfDependency
          <|> Map.lookup (canonicalPkgName name) mapOfDependency
