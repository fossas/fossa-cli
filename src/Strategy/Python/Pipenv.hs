{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Strategy.Python.Pipenv
  ( discover
  , findProjects
  , getDeps
  , mkProject

  , PipenvGraphDep(..)
  , PipfileLock(..)
  , PipfileMeta(..)
  , PipfileSource(..)
  , PipfileDep(..)
  , buildGraph
  )
  where

import Control.Effect.Diagnostics
import Data.Aeson
import Data.Foldable (for_, traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = map mkProject <$> findProjects dir

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [PipenvProject]
findProjects = walk' $ \_ _ files -> do
  case findFileNamed "Pipfile.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([PipenvProject file], WalkContinue)

getDeps ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  )
  => PipenvProject -> m (Graphing Dependency)
getDeps project = do
  lock <- readContentsJson (pipenvLockfile project)
  maybeDeps <- recover $ execJson (parent (pipenvLockfile project)) pipenvGraphCmd

  pure (buildGraph lock maybeDeps)

mkProject :: (Has ReadFS sig n, Has Exec sig n, Has Diagnostics sig n) => PipenvProject -> DiscoveredProject n
mkProject project = DiscoveredProject
  { projectType = "pipenv"
  , projectBuildTargets = mempty
  , projectDependencyGraph = const $ getDeps project
  , projectPath = parent $ pipenvLockfile project
  , projectLicenses = pure []
  }

newtype PipenvProject = PipenvProject
  { pipenvLockfile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

pipenvGraphCmd :: Command
pipenvGraphCmd = Command
  { cmdName = "pipenv"
  , cmdArgs = ["graph", "--json-tree"]
  , cmdAllowErr = Never
  }

buildGraph :: PipfileLock -> Maybe [PipenvGraphDep] -> Graphing Dependency
buildGraph lock maybeDeps = run . withLabeling toDependency $ do
  buildNodes lock
  case maybeDeps of
    Just deps -> buildEdges deps
    Nothing -> pure ()

  where
  toDependency :: PipPkg -> Set PipLabel -> Dependency
  toDependency pkg = foldr applyLabel start
    where
    applyLabel :: PipLabel -> Dependency -> Dependency
    applyLabel (PipSource loc) dep = dep { dependencyLocations = loc : dependencyLocations dep }
    applyLabel (PipEnvironment env) dep = dep { dependencyEnvironments = env : dependencyEnvironments dep }

    start = Dependency
      { dependencyType = PipType
      , dependencyName = pipPkgName pkg
      , dependencyVersion = CEq <$> pipPkgVersion pkg
      , dependencyLocations = []
      , dependencyEnvironments = []
      , dependencyTags = M.empty
      }

data PipPkg = PipPkg
  { pipPkgName    :: Text
  , pipPkgVersion :: Maybe Text
  } deriving (Eq, Ord, Show)

type PipGrapher = LabeledGrapher PipPkg PipLabel

data PipLabel =
    PipSource Text -- location
  | PipEnvironment DepEnvironment
  deriving (Eq, Ord, Show)

buildNodes :: forall sig m. Has PipGrapher sig m => PipfileLock -> m ()
buildNodes PipfileLock{..} = do
  let indexBy :: Ord k => (v -> k) -> [v] -> Map k v
      indexBy ix = M.fromList . map (\v -> (ix v, v))

      sourcesMap = indexBy sourceName (fileSources fileMeta)

  _ <- M.traverseWithKey (addWithEnv EnvDevelopment sourcesMap) fileDevelop
  _ <- M.traverseWithKey (addWithEnv EnvProduction sourcesMap) fileDefault
  pure ()

  where

  addWithEnv :: DepEnvironment
               -> Map Text PipfileSource
               -> Text -- dep name
               -> PipfileDep
               -> m ()
  addWithEnv env sourcesMap depName dep = do
    let pkg = PipPkg depName (T.drop 2 <$> fileDepVersion dep)
    -- TODO: reachable instead of direct
    direct pkg
    label pkg (PipEnvironment env)

    -- add label for source when it exists
    for_ (fileDepIndex dep) $ \index ->
      case M.lookup index sourcesMap of
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
  { fileMeta    :: PipfileMeta
  , fileDefault :: Map Text PipfileDep
  , fileDevelop :: Map Text PipfileDep
  } deriving (Eq, Ord, Show)

newtype PipfileMeta = PipfileMeta
  { fileSources :: [PipfileSource]
  } deriving (Eq, Ord, Show)

data PipfileSource = PipfileSource
  { sourceName :: Text
  , sourceUrl  :: Text
  } deriving (Eq, Ord, Show)

data PipfileDep = PipfileDep
  { fileDepVersion :: Maybe Text
  , fileDepIndex   :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromJSON PipfileLock where
  parseJSON = withObject "PipfileLock" $ \obj ->
    PipfileLock <$> obj .: "_meta"
                <*> obj .: "default"
                <*> obj .: "develop"

instance FromJSON PipfileDep where
  parseJSON = withObject "PipfileDep" $ \obj ->
    PipfileDep <$> obj .:? "version"
               <*> obj .:? "index"

instance FromJSON PipfileMeta where
  parseJSON = withObject "PipfileMeta" $ \obj ->
    PipfileMeta <$> obj .: "sources"

instance FromJSON PipfileSource where
  parseJSON = withObject "PipfileSource" $ \obj ->
    PipfileSource <$> obj .: "name"
                  <*> obj .: "url"

---------- pipenv graph

data PipenvGraphDep = PipenvGraphDep
  { depName         :: Text
  , depInstalled    :: Text
  , depRequired     :: Text
  , depDependencies :: [PipenvGraphDep]
  } deriving (Eq, Ord, Show)

instance FromJSON PipenvGraphDep where
  parseJSON = withObject "PipenvGraphDep" $ \obj ->
    PipenvGraphDep <$> obj .: "package_name"
                   <*> obj .: "installed_version"
                   <*> obj .: "required_version"
                   <*> obj .: "dependencies"
