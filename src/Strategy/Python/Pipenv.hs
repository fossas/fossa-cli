module Strategy.Python.Pipenv
  ( discover
  , analyze

  , PipenvGraphDep(..)
  , PipfileLock(..)
  , PipfileMeta(..)
  , PipfileSource(..)
  , PipfileDep(..)
  , buildGraph
  )
  where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Discovery.Walk
import DepTypes
import Graphing (Graphing)
import Effect.Exec
import Effect.LabeledGrapher
import Effect.ReadFS
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "Pipfile.lock") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "python-pipenv" PythonGroup $ analyze file

  walkContinue

pipenvGraphCmd :: Command
pipenvGraphCmd = Command
  { cmdNames = ["pipenv"]
  , cmdBaseArgs = ["graph", "--json-tree"]
  , cmdAllowErr = Never
  }

analyze ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has (Error ReadFSErr) sig m
  , Effect sig
  )
  => Path Rel File -> m ProjectClosureBody
analyze lockfile = do
  lock <- readContentsJson lockfile
  -- TODO: diagnostics?
  maybeDeps <- runError @ExecErr (execJson (parent lockfile) pipenvGraphCmd [])

  pure (mkProjectClosure lockfile lock (eitherToMaybe maybeDeps))

mkProjectClosure :: Path Rel File -> PipfileLock -> Maybe [PipenvGraphDep] -> ProjectClosureBody
mkProjectClosure file lockfile maybeDeps = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph lockfile maybeDeps
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

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
      , dependencyVersion = Just (CEq (pipPkgVersion pkg))
      , dependencyLocations = []
      , dependencyEnvironments = []
      , dependencyTags = M.empty
      }

data PipPkg = PipPkg
  { pipPkgName    :: Text
  , pipPkgVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

type instance PkgLabel PipPkg = PipLabel

data PipLabel =
    PipSource Text -- location
  | PipEnvironment DepEnvironment
  deriving (Eq, Ord, Show, Generic)

buildNodes :: forall sig m. Has (LabeledGrapher PipPkg) sig m => PipfileLock -> m ()
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
    let pkg = PipPkg depName (T.drop 2 (fileDepVersion dep))
    -- TODO: reachable instead of direct
    direct pkg
    label pkg (PipEnvironment env)

    -- add label for source when it exists
    for_ (fileDepIndex dep) $ \index ->
      case M.lookup index sourcesMap of
        Just source -> label pkg (PipSource (sourceUrl source))
        Nothing -> pure ()

buildEdges :: Has (LabeledGrapher PipPkg) sig m => [PipenvGraphDep] -> m ()
buildEdges pipenvDeps = do
  traverse_ (direct . mkPkg) pipenvDeps
  traverse_ mkEdges pipenvDeps

  where

  mkPkg :: PipenvGraphDep -> PipPkg
  mkPkg dep = PipPkg (depName dep) (depInstalled dep)

  mkEdges :: Has (LabeledGrapher PipPkg) sig m => PipenvGraphDep -> m ()
  mkEdges parentDep =
    forM_ (depDependencies parentDep) $ \childDep -> do
      edge (mkPkg parentDep) (mkPkg childDep)
      mkEdges childDep

---------- Pipfile.lock

data PipfileLock = PipfileLock
  { fileMeta    :: PipfileMeta
  , fileDefault :: Map Text PipfileDep
  , fileDevelop :: Map Text PipfileDep
  } deriving (Eq, Ord, Show, Generic)

newtype PipfileMeta = PipfileMeta
  { fileSources :: [PipfileSource]
  } deriving (Eq, Ord, Show, Generic)

data PipfileSource = PipfileSource
  { sourceName :: Text
  , sourceUrl  :: Text
  } deriving (Eq, Ord, Show, Generic)

data PipfileDep = PipfileDep
  { fileDepVersion :: Text
  , fileDepIndex   :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PipfileLock where
  parseJSON = withObject "PipfileLock" $ \obj ->
    PipfileLock <$> obj .: "_meta"
                <*> obj .: "default"
                <*> obj .: "develop"

instance FromJSON PipfileDep where
  parseJSON = withObject "PipfileDep" $ \obj ->
    PipfileDep <$> obj .:  "version"
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
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PipenvGraphDep where
  parseJSON = withObject "PipenvGraphDep" $ \obj ->
    PipenvGraphDep <$> obj .: "package_name"
                   <*> obj .: "installed_version"
                   <*> obj .: "required_version"
                   <*> obj .: "dependencies"
