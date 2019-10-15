
{-# language QuasiQuotes #-}

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
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Polysemy.Output

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
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "Pipfile.lock") files of
    Nothing -> walkContinue
    Just file -> do
      output (configureWithCmd file)
      output (configureNoCmd file)
      walkContinue

pipenvGraphCmd :: Command
pipenvGraphCmd = Command
  { cmdNames = [[relfile|pipenv|]]
  , cmdArgs = ["graph", "--json-tree"]
  }

strategyWithCmd :: Strategy BasicFileOpts
strategyWithCmd = Strategy
  { strategyName = "python-pipenv"
  , strategyAnalyze = \opts -> analyzeWithCmd
      & fileInputJson @PipfileLock (targetFile opts)
      & execInputJson @[PipenvGraphDep] (parent (targetFile opts)) pipenvGraphCmd
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

strategyNoCmd :: Strategy BasicFileOpts
strategyNoCmd = Strategy
  { strategyName = "python-pipfile"
  , strategyAnalyze = \opts -> analyzeNoCmd & fileInputJson (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

configureWithCmd :: Path Rel File -> ConfiguredStrategy
configureWithCmd = ConfiguredStrategy strategyWithCmd . BasicFileOpts

configureNoCmd :: Path Rel File -> ConfiguredStrategy
configureNoCmd = ConfiguredStrategy strategyNoCmd . BasicFileOpts

analyzeWithCmd :: Members '[Input PipfileLock, Input [PipenvGraphDep]] r => Sem r G.Graph
analyzeWithCmd = buildGraph <$> input <*> (Just <$> input)

analyzeNoCmd :: Member (Input PipfileLock) r => Sem r G.Graph
analyzeNoCmd = buildGraph <$> input <*> pure Nothing

buildGraph :: PipfileLock -> Maybe [PipenvGraphDep] -> G.Graph
buildGraph lock Nothing =
  let (nodesByName, graph) = buildNodes lock
   in foldr G.addDirect graph nodesByName
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
      , dependencyVersion = Just (G.CEq (T.drop 2 fileDepVersion)) -- fileDepVersion starts with "=="
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
  mkDirect dep =
    case M.lookup (depName dep) depNameToRef of
      Just ref -> addDirect ref
      Nothing -> pure ()

  mkEdges :: Member GraphBuilder r => PipenvGraphDep -> Sem r ()
  mkEdges parentDep =
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
