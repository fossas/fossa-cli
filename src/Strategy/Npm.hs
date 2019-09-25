module Strategy.Npm
  ( discover
  , strategy
  , analyze
  , configure
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Foldable hiding (find, fold)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           GHC.Generics
import           Path
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           System.Exit
import           System.FilePath.Find

import           Config
import           Effect.Exec
import           Effect.ReadFS
import           Graph (Graph)
import qualified Graph as G
import           Strategy

data NpmOpts = NpmOpts
  { npmOptsDir :: !(Path Rel Dir)
  } deriving Show

instance FromJSON NpmOpts where
  parseJSON = withObject "NpmOpts" $ \obj -> NpmOpts <$> obj .: "dir"

instance ToJSON NpmOpts where
  toJSON NpmOpts{..} = object ["dir" .= npmOptsDir]

discover :: (Member (Embed IO) r, Member ReadFS r) => Path Abs Dir -> Sem r [ConfiguredStrategy]
discover basedir = do
  paths <- embed $ find always (fileName ==? "package.json") (toFilePath basedir)
  files <- embed @IO $ traverse (stripProperPrefix basedir <=< parseAbsFile) paths
  pure $ map (configure . parent) files

strategy :: Strategy NpmOpts
strategy = Strategy
  { strategyName = "nodejs-npm"
  , strategyAnalyze = \opts -> do
      result <- runFinal . embedToFinal @IO . execToIO . errorToIOFinal @String . stateToIO @Graph G.empty . analyze $ opts
      pure (fmap fst result)
  }

analyze :: Members '[Exec, Error String, State Graph] r => NpmOpts -> Sem r ()
analyze NpmOpts{..} = do
  (exitcode, stdout, _) <- exec npmOptsDir "npm" ["ls", "--json", "--production"]
  when (exitcode /= ExitSuccess) (throw @String "NPM returned an error code")
  either throw buildGraph (eitherDecode stdout)

state :: Member (State s) r => (s -> (a,s)) -> Sem r a
state f = do
  before <- get
  let (result, after) = f before
  put after
  pure result

buildGraph :: Member (State Graph) r => NpmOutput -> Sem r ()
buildGraph top = do
  topLevel <- M.traverseWithKey buildNode (outputDependencies top)
  traverse_ (modify . G.addDirect) topLevel

  where

  buildNode :: Member (State Graph) r => Text -> NpmOutput -> Sem r G.DepRef
  buildNode nodeName nodeOutput = do
    children <- M.traverseWithKey buildNode (outputDependencies nodeOutput)
    parentRef <- state $ G.addNode $ G.Dependency
      { dependencyType = G.NodeJSType
      , dependencyName = nodeName
      , dependencyVersion = outputVersion nodeOutput
      , dependencyLocations = []
      }
    traverse_ (modify . G.addEdge parentRef) children
    pure parentRef

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . NpmOpts

data NpmOutput = NpmOutput
  { outputVersion      :: Text
  , outputFrom         :: Maybe Text
  , outputResolved     :: Maybe Text
  , outputDependencies :: Map Text NpmOutput
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON NpmOutput where
  parseJSON = withObject "Output" $ \obj ->
    NpmOutput <$> obj .:  "version"
              <*> obj .:? "from"
              <*> obj .:? "resolved"
              <*> obj .:? "dependencies" .!= M.empty
