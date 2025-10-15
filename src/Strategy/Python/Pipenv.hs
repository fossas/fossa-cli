{-# LANGUAGE RecordWildCards #-}

module Strategy.Python.Pipenv (
  discover,
  findProjects,
  getDeps,
  mkProject,
  PipenvGraphDep (..),
  PipfileLock (..),
  PipfileToml (..),
  PipfileMeta (..),
  PipfileSource (..),
  PipfileDep (..),
  PipenvProject (..),
  buildGraph,
  pipfilePackageList,
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
import Control.Monad (join)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON,
  withObject,
  (.:),
  (.:?),
 )
import Data.Bifunctor (bimap)
import Data.Foldable (for_, traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
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
import Diag.Common (
  MissingDeepDeps (MissingDeepDeps),
  MissingEdges (MissingEdges),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson)
import Effect.Grapher (
  LabeledGrapher,
  deep,
  direct,
  edge,
  label,
  withLabeling,
 )
import Effect.ReadFS (ReadFS, readContentsJson, readContentsToml)
import GHC.Generics (Generic)
import Graphing (Graphing, pruneUnreachable)
import Path (Abs, Dir, File, Path, parent)
import Strategy.Python.Errors (PipenvCmdFailed (..))
import Toml.Schema qualified
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (PipenvProjectType),
  GraphBreadth (Complete),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject PipenvProject]
discover = simpleDiscover findProjects mkProject PipenvProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [PipenvProject]
findProjects = walkWithFilters' $ \_ _ files -> do
  case findPipenvFiles files of
    (Nothing, _) -> pure ([], WalkContinue)
    (_, Nothing) -> pure ([], WalkContinue)
    (Just pipfile, Just lock) -> pure ([PipenvProject pipfile lock], WalkContinue)
  where
    findPipenvFiles files = join bimap (\f -> findFileNamed f files) ("Pipfile", "Pipfile.lock")

getDeps ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  PipenvProject ->
  m DependencyResults
getDeps project = context "Pipenv" $ do
  lock <- context "Getting dependencies from Pipfile.lock" $ readContentsJson (pipenvLockfile project)
  pipfile <- context "Getting dependencies from Pipfile" $ readContentsToml (pipenvPipfile project)

  maybeDeps <-
    context "Getting deep dependencies"
      $ recover
        . warnOnErr MissingDeepDeps
        . warnOnErr MissingEdges
        . errCtx (PipenvCmdFailedCtx pipenvGraphCmd)
        . errHelp PipenvCmdFailedHelp
      $ execJson (parent (pipenvLockfile project)) pipenvGraphCmd

  graph <- context "Building dependency graph" $ pure (buildGraph pipfile lock maybeDeps)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [pipenvLockfile project]
      }

getDepsStatically ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  PipenvProject ->
  m DependencyResults
getDepsStatically project = context "Pipenv" $ do
  lock <- context "Getting dependencies from Pipfile.lock" $ readContentsJson (pipenvLockfile project)
  pipfile <- context "Getting dependencies from Pipfile" $ readContentsToml (pipenvPipfile project)
  graph <- context "Building dependency graph" $ pure (buildGraph pipfile lock Nothing)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [pipenvLockfile project]
      }

mkProject :: PipenvProject -> DiscoveredProject PipenvProject
mkProject project =
  DiscoveredProject
    { projectType = PipenvProjectType
    , projectBuildTargets = mempty
    , projectPath = parent $ pipenvLockfile project
    , projectData = project
    }

data PipenvProject = PipenvProject
  { pipenvPipfile :: Path Abs File
  , pipenvLockfile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PipenvProject

instance AnalyzeProject PipenvProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDepsStatically

pipenvGraphCmd :: Command
pipenvGraphCmd =
  Command
    { cmdName = "pipenv"
    , cmdArgs = ["graph", "--json-tree"]
    , cmdAllowErr = Never
    }

buildGraph :: PipfileToml -> PipfileLock -> Maybe [PipenvGraphDep] -> Graphing Dependency
buildGraph pipfile lock maybeDeps = prune $ run . withLabeling toDependency $ do
  buildNodes pipfile lock
  traverse_ buildEdges maybeDeps
  where
    toDependency :: PipPkg -> Set PipLabel -> Dependency
    toDependency pkg = foldr applyLabel start
      where
        applyLabel :: PipLabel -> Dependency -> Dependency
        applyLabel (PipSource loc) = insertLocation loc
        applyLabel (PipEnvironment env) = insertEnvironment env

        start =
          Dependency
            { dependencyType = PipType
            , dependencyName = pipPkgName pkg
            , dependencyVersion = CEq <$> pipPkgVersion pkg
            , dependencyLocations = []
            , dependencyEnvironments = mempty
            , dependencyTags = Map.empty
            }

    -- We only have edges if we have graph dependencies. Only prune unreachable in this case
    prune = if isJust maybeDeps then pruneUnreachable else id

data PipPkg = PipPkg
  { pipPkgName :: Text
  , pipPkgVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

type PipGrapher = LabeledGrapher PipPkg PipLabel

data PipLabel
  = PipSource Text -- location
  | PipEnvironment DepEnvironment
  deriving (Eq, Ord, Show)

buildNodes :: forall sig m. Has PipGrapher sig m => PipfileToml -> PipfileLock -> m ()
buildNodes PipfileToml{..} PipfileLock{..} = do
  let indexBy :: Ord k => (v -> k) -> [v] -> Map k v
      indexBy ix = Map.fromList . map (\v -> (ix v, v))

      sourcesMap = indexBy sourceName (fileSources fileMeta)

  _ <- Map.traverseWithKey (addWithEnv EnvDevelopment sourcesMap) fileDevelop
  _ <- Map.traverseWithKey (addWithEnv EnvProduction sourcesMap) fileDefault
  pure ()
  where
    addWithEnv ::
      DepEnvironment ->
      Map Text PipfileSource ->
      Text -> -- dep name
      PipfileDep ->
      m ()
    addWithEnv env sourcesMap name dep = do
      let pkg = PipPkg name (Text.drop 2 <$> fileDepVersion dep)
      -- If we don't have graph deps, we're unable to distinguish between direct and transitive dependencies,
      -- so in that case we mark every dependency as direct
      -- If we do have graph deps, we set direct dependencies in buildEdges so treat the dependencies as deep here
      let graphFn = if Map.member name pipfilePackages || Map.member name pipfileDevPackages then direct else deep
      graphFn pkg
      label pkg (PipEnvironment env)

      -- add label for source when it exists
      for_ (fileDepIndex dep) $ \index ->
        case Map.lookup index sourcesMap of
          Just source -> label pkg (PipSource (sourceUrl source))
          Nothing -> pure ()

buildEdges :: Has PipGrapher sig m => [PipenvGraphDep] -> m ()
buildEdges pipenvDeps = do
  traverse_ (direct . mkPkg) pipenvDeps
  traverse_ mkEdges pipenvDeps
  where
    mkPkg :: PipenvGraphDep -> PipPkg
    mkPkg dep = PipPkg (depName dep) $ Just (depInstalled dep)

    mkEdges :: Has PipGrapher sig m => PipenvGraphDep -> m ()
    mkEdges parentDep =
      for_ (depDependencies parentDep) $ \childDep -> do
        edge (mkPkg parentDep) (mkPkg childDep)
        mkEdges childDep

---------- Pipfile.lock

data PipfileLock = PipfileLock
  { fileMeta :: PipfileMeta
  , fileDefault :: Map Text PipfileDep
  , fileDevelop :: Map Text PipfileDep
  }
  deriving (Eq, Ord, Show)

newtype PipfileMeta = PipfileMeta
  { fileSources :: [PipfileSource]
  }
  deriving (Eq, Ord, Show)

data PipfileSource = PipfileSource
  { sourceName :: Text
  , sourceUrl :: Text
  }
  deriving (Eq, Ord, Show)

data PipfileDep = PipfileDep
  { fileDepVersion :: Maybe Text
  , fileDepIndex :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON PipfileLock where
  parseJSON = withObject "PipfileLock" $ \obj ->
    PipfileLock
      <$> obj .: "_meta"
      <*> obj .: "default"
      <*> obj .: "develop"

instance FromJSON PipfileDep where
  parseJSON = withObject "PipfileDep" $ \obj ->
    PipfileDep
      <$> obj .:? "version"
      <*> obj .:? "index"

instance FromJSON PipfileMeta where
  parseJSON = withObject "PipfileMeta" $ \obj ->
    PipfileMeta <$> obj .: "sources"

instance FromJSON PipfileSource where
  parseJSON = withObject "PipfileSource" $ \obj ->
    PipfileSource
      <$> obj .: "name"
      <*> obj .: "url"

---------- pipenv graph

data PipenvGraphDep = PipenvGraphDep
  { depName :: Text
  , depInstalled :: Text
  , depRequired :: Text
  , depDependencies :: [PipenvGraphDep]
  }
  deriving (Eq, Ord, Show)

instance FromJSON PipenvGraphDep where
  parseJSON = withObject "PipenvGraphDep" $ \obj ->
    PipenvGraphDep
      <$> obj .: "package_name"
      <*> obj .: "installed_version"
      <*> obj .: "required_version"
      <*> obj .: "dependencies"

---------- Pipfile

data PipfileToml = PipfileToml
  -- {pipfilePackages :: Map Text (), pipfileDevPackages :: Map Text ()}
  {pipfilePackages :: Map Text PipfilePackageVersion, pipfileDevPackages :: Map Text PipfilePackageVersion}
  deriving (Eq, Show)

instance Toml.Schema.FromValue PipfileToml where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PipfileToml
        <$> Toml.Schema.pickKey [Toml.Schema.Key "packages" Toml.Schema.fromValue, Toml.Schema.Else (pure mempty)]
        <*> Toml.Schema.pickKey [Toml.Schema.Key "dev-packages" Toml.Schema.fromValue, Toml.Schema.Else (pure mempty)]

-- We don't need to parse the package versions in the Pipfile as we get version info from the lock file
data PipfilePackageVersion = PipfilePackageVersion deriving (Eq, Ord, Show)
instance Toml.Schema.FromValue PipfilePackageVersion where
  fromValue _ = pure PipfilePackageVersion

pipfilePackageList :: [Text] -> Map Text PipfilePackageVersion
pipfilePackageList = Map.fromList . map (,PipfilePackageVersion)
