
module Strategy.Python.Pipenv
  ( discover
  , strategy
  , analyze
  , configure

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
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Polysemy.Output

import           Diagnostics
import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import           Effect.ReadFS
import qualified Graph as G
import           Types

discover :: Discover
discover = Discover
  { discoverName = "pipenv"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "Pipfile.lock") files of
    Nothing -> walkContinue
    Just file -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "python-pipenv"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts

analyze :: Members '[Exec, ReadFS, Error CLIErr] r => BasicFileOpts -> Sem r G.Graph
analyze BasicFileOpts{..} = do
  contents <- readContentsBS targetFile

  case eitherDecodeStrict contents of
    Left err -> throw $ FileParseError (toFilePath targetFile) $ show err
    Right pipfileLock -> do
      maybePipenvDeps <- pipenvGraph (parent targetFile)
      pure (buildGraph pipfileLock maybePipenvDeps)

buildGraph :: PipfileLock -> Maybe [PipenvGraphDep] -> G.Graph
buildGraph lock Nothing =
  let (nodesByName, graph) = buildNodes lock
   in foldr (\dep graph' -> G.addDirect dep graph') graph nodesByName
buildGraph lock (Just deps) =
  let (nodesByName, graph) = buildNodes lock
   in buildEdges nodesByName deps graph

-- TODO: don't add direct here?
buildNodes :: PipfileLock -> (Map Text G.DepRef, G.Graph)
buildNodes PipfileLock{..} = run . runState M.empty . evalGraphBuilder G.empty $ do
  let indexBy :: Ord k => (v -> k) -> [v] -> Map k v
      indexBy ix = M.fromList . map (\v -> (ix v, v))

      sourcesMap = indexBy sourceName (fileSources fileMeta)

  -- NB: the order here is important: production refs will take priority over
  -- development refs in the output graph
  -- alternative approach: try to dedupe dev and prod dependencies?
  _ <- M.traverseWithKey (mkNode sourcesMap "development") fileDevelop
  _ <- M.traverseWithKey (mkNode sourcesMap "production") fileDefault
  pure ()

  where

  mkNode :: Members '[GraphBuilder, State (Map Text G.DepRef)] r
         => Map Text PipfileSource
         -> Text -- env name: production, develop
         -> Text -- dep name
         -> PipfileDep
         -> Sem r G.DepRef
  mkNode sourcesMap env depName PipfileDep{..} = do
    ref <- addNode $ G.Dependency
      { dependencyType = G.PipType
      , dependencyName = depName
      , dependencyVersion = Just (T.drop 2 fileDepVersion) -- fileDepVersion starts with "=="
      , dependencyLocations =
          case fileDepIndex of
            Nothing -> []
            Just index -> case M.lookup index sourcesMap of
              Nothing -> []
              Just source -> [sourceUrl source]
      , dependencyTags = M.singleton "environment" [env]
      }
    modify (M.insert depName ref)
    pure ref

buildEdges :: Map Text G.DepRef -> [PipenvGraphDep] -> G.Graph -> G.Graph
buildEdges depNameToRef pipenvDeps graph = run . evalGraphBuilder graph $ do
  traverse_ mkEdges pipenvDeps
  traverse_ mkDirect pipenvDeps -- add top level deps as "direct"

  where

  mkDirect :: Member GraphBuilder r => PipenvGraphDep -> Sem r ()
  mkDirect dep = do
    case M.lookup (depName dep) depNameToRef of
      Just ref -> addDirect ref
      Nothing -> pure ()

  mkEdges :: Member GraphBuilder r => PipenvGraphDep -> Sem r ()
  mkEdges parentDep = do
    forM_ (depDependencies parentDep) $ \childDep -> do
      let maybeParentRef = M.lookup (depName parentDep) depNameToRef
          maybeChildRef = M.lookup (depName childDep) depNameToRef

      case (maybeParentRef, maybeChildRef) of
        (Just parentRef, Just childDepRef) -> addEdge parentRef childDepRef
        _ -> pure ()

      mkEdges childDep

---------- Pipfile.lock

data PipfileLock = PipfileLock
  { fileMeta    :: PipfileMeta
  , fileDefault :: Map Text PipfileDep
  , fileDevelop :: Map Text PipfileDep
  } deriving (Eq, Ord, Show, Generic)

data PipfileMeta = PipfileMeta
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

pipenvGraph :: Member Exec r => Path Rel Dir -> Sem r (Maybe [PipenvGraphDep])
pipenvGraph dir = do
  (exitcode, stdout, _) <- exec dir "pipenv" ["graph", "--json-tree"]
  case (exitcode, eitherDecode stdout) of
    (ExitFailure _, _) -> pure Nothing -- TODO: warning?
    (_,        Left _) -> pure Nothing -- TODO: warning?
    (ExitSuccess, Right a) -> pure (Just a)

data PipenvGraphDep = PipenvGraphDep
  { depName         :: Text
  , depInstalled    :: Text
  , depRequired     :: Text
  , depDependencies :: [PipenvGraphDep]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PipenvGraphDep where
  parseJSON = withObject "PipenvGraphDep" $ \obj -> do
    PipenvGraphDep <$> obj .: "package_name"
                   <*> obj .: "installed_version"
                   <*> obj .: "required_version"
                   <*> obj .: "dependencies"
