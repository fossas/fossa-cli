{-# LANGUAGE RecordWildCards #-}

module Strategy.Python.Pipenv (
  discover,
  findProjects,
  getDeps,
  mkProject,
  PipenvGraphDep (..),
  PipfileLock (..),
  PipfileMeta (..),
  PipfileSource (..),
  PipfileDep (..),
  PipenvProject (..),
  buildGraph,
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
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON,
  withObject,
  (.:),
  (.:?),
 )
import Data.Foldable (for_, traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
  direct,
  edge,
  label,
  withLabeling,
 )
import Effect.ReadFS (ReadFS, readContentsJson)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, parent)
import Strategy.Python.Errors (PipenvCmdFailed (..))
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
  case findFileNamed "Pipfile.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([PipenvProject file], WalkContinue)

getDeps ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  PipenvProject ->
  m DependencyResults
getDeps project = context "Pipenv" $ do
  lock <- context "Getting direct dependencies" $ readContentsJson (pipenvLockfile project)

  maybeDeps <-
    context "Getting deep dependencies"
      $ recover
        . warnOnErr MissingDeepDeps
        . warnOnErr MissingEdges
        . errCtx (PipenvCmdFailedCtx pipenvGraphCmd)
        . errHelp PipenvCmdFailedHelp
      $ execJson (parent (pipenvLockfile project)) pipenvGraphCmd

  graph <- context "Building dependency graph" $ pure (buildGraph lock maybeDeps)
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
  lock <- context "Getting direct dependencies" $ readContentsJson (pipenvLockfile project)
  graph <- context "Building dependency graph" $ pure (buildGraph lock Nothing)
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

newtype PipenvProject = PipenvProject
  { pipenvLockfile :: Path Abs File
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

buildGraph :: PipfileLock -> Maybe [PipenvGraphDep] -> Graphing Dependency
buildGraph lock maybeDeps = run . withLabeling toDependency $ do
  buildNodes lock
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

buildNodes :: forall sig m. Has PipGrapher sig m => PipfileLock -> m ()
buildNodes PipfileLock{..} = do
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
    addWithEnv env sourcesMap depName dep = do
      let pkg = PipPkg depName (Text.drop 2 <$> fileDepVersion dep)
      -- TODO: reachable instead of direct
      direct pkg
      label pkg (PipEnvironment env)

      -- add label for source when it exists
      for_ (fileDepIndex dep) $ \index ->
        case Map.lookup index sourcesMap of
          Just source -> label pkg (PipSource (sourceUrl source))
          Nothing -> pure ()

buildEdges :: Has PipGrapher sig m => [PipenvGraphDep] -> m ()
buildEdges pipenvDeps = do
  -- Only mark top-level deps as direct
  traverse_ (direct . mkPkg) pipenvDeps
  -- Build edges for the dependency tree
  traverse_ mkEdgesRec pipenvDeps
  where
    mkPkg :: PipenvGraphDep -> PipPkg
    mkPkg dep = PipPkg (depName dep) $ Just (depInstalled dep)

    -- Helper to recursively build edges through the dependency tree
    mkEdgesRec :: Has PipGrapher sig m => PipenvGraphDep -> m ()
    mkEdgesRec parentDep =
      for_ (depDependencies parentDep) $ \childDep -> do
        -- Create edge between parent and child
        edge (mkPkg parentDep) (mkPkg childDep)
        -- Process child's dependencies
        mkEdgesRec childDep

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
