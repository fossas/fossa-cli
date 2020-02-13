module Strategy.Python.Pipenv
  ( discover
  , strategyWithCmd
  , strategyNoCmd
  , analyzeWithCmd
  , analyzeNoCmd
  , configureWithCmd
  , configureNoCmd

  , PipenvGraphDep(..)
  , PipfileLock(..)
  , PipfileMeta(..)
  , PipfileSource(..)
  , PipfileDep(..)
  , buildGraph
  )
  where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Polysemy
import Polysemy.Input
import Polysemy.Output

import Discovery.Walk
import DepTypes
import qualified Graphing as G
import Effect.Exec
import Effect.LabeledGrapher
import Effect.ReadFS
import Types

discover :: Discover
discover = Discover
  { discoverName = "pipenv"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "Pipfile.lock") files of
    Nothing -> walkContinue
    Just file -> do
      output (configureWithCmd file)
      output (configureNoCmd file)
      walkContinue

pipenvGraphCmd :: Command
pipenvGraphCmd = Command
  { cmdNames = ["pipenv"]
  , cmdBaseArgs = ["graph", "--json-tree"]
  , cmdAllowErr = Never
  }

strategyWithCmd :: Strategy BasicFileOpts
strategyWithCmd = Strategy
  { strategyName = "python-pipenv"
  , strategyAnalyze = \opts -> analyzeWithCmd
      & fileInputJson @PipfileLock (targetFile opts)
      & execInputJson @[PipenvGraphDep] (parent (targetFile opts)) pipenvGraphCmd []
  , strategyLicense = const (pure [])
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

strategyNoCmd :: Strategy BasicFileOpts
strategyNoCmd = Strategy
  { strategyName = "python-pipfile"
  , strategyAnalyze = \opts -> analyzeNoCmd & fileInputJson @PipfileLock (targetFile opts)
  , strategyLicense = const (pure [])
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

configureWithCmd :: Path Rel File -> ConfiguredStrategy
configureWithCmd = ConfiguredStrategy strategyWithCmd . BasicFileOpts

configureNoCmd :: Path Rel File -> ConfiguredStrategy
configureNoCmd = ConfiguredStrategy strategyNoCmd . BasicFileOpts

analyzeWithCmd :: Members '[Input PipfileLock, Input [PipenvGraphDep]] r => Sem r (G.Graphing Dependency)
analyzeWithCmd = buildGraph <$> input <*> (Just <$> input)

analyzeNoCmd :: Member (Input PipfileLock) r => Sem r (G.Graphing Dependency)
analyzeNoCmd = buildGraph <$> input <*> pure Nothing

buildGraph :: PipfileLock -> Maybe [PipenvGraphDep] -> G.Graphing Dependency
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
    applyLabel (PipEnvironment env) dep = dep { dependencyTags = M.insertWith (<>) "environment" [env] (dependencyTags dep) }

    start = Dependency
      { dependencyType = PipType
      , dependencyName = pipPkgName pkg
      , dependencyVersion = Just (CEq (pipPkgVersion pkg))
      , dependencyLocations = []
      , dependencyTags = M.empty
      }

data PipPkg = PipPkg
  { pipPkgName    :: Text
  , pipPkgVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

type instance PkgLabel PipPkg = PipLabel

data PipLabel =
    PipSource Text -- location
  | PipEnvironment Text -- production, development
  deriving (Eq, Ord, Show, Generic)

buildNodes :: Member (LabeledGrapher PipPkg) r => PipfileLock -> Sem r ()
buildNodes PipfileLock{..} = do
  let indexBy :: Ord k => (v -> k) -> [v] -> Map k v
      indexBy ix = M.fromList . map (\v -> (ix v, v))

      sourcesMap = indexBy sourceName (fileSources fileMeta)

  _ <- M.traverseWithKey (addWithLabel "development" sourcesMap) fileDevelop
  _ <- M.traverseWithKey (addWithLabel "production" sourcesMap) fileDefault
  pure ()

  where

  addWithLabel :: Member (LabeledGrapher PipPkg) r
               => Text -- env name: production, development
               -> Map Text PipfileSource
               -> Text -- dep name
               -> PipfileDep
               -> Sem r ()
  addWithLabel env sourcesMap depName dep = do
    let pkg = PipPkg depName (T.drop 2 (fileDepVersion dep))
    -- TODO: reachable instead of direct
    direct pkg
    label pkg (PipEnvironment env)

    -- add label for source when it exists
    for_ (fileDepIndex dep) $ \index ->
      case M.lookup index sourcesMap of
        Just source -> label pkg (PipSource (sourceUrl source))
        Nothing -> pure ()

buildEdges :: Member (LabeledGrapher PipPkg) r => [PipenvGraphDep] -> Sem r ()
buildEdges pipenvDeps = do
  traverse_ (direct . mkPkg) pipenvDeps
  traverse_ mkEdges pipenvDeps

  where

  mkPkg :: PipenvGraphDep -> PipPkg
  mkPkg dep = PipPkg (depName dep) (depInstalled dep)

  mkEdges :: Member (LabeledGrapher PipPkg) r => PipenvGraphDep -> Sem r ()
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
