module Strategy.Npm
  ( discover
  , strategy
  , analyze
  , configure
  ) where

import Prologue

import           Data.ByteString.Lazy.Optics
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Optics
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           System.Exit
import           System.FilePath.Find

import           Config
import           Effect.ErrorTrace
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy

data NpmOpts = NpmOpts
  { npmOptsDir :: Path Rel Dir
  } deriving Show

instance FromJSON NpmOpts where
  parseJSON = withObject "NpmOpts" $ \obj -> NpmOpts <$> obj .: "dir"

instance ToJSON NpmOpts where
  toJSON NpmOpts{..} = object ["dir" .= npmOptsDir]

-- TODO: swap out Embed IO for a FSWalk effect
discover :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover basedir = do
  paths <- embed $ find (fileName /~? "node_modules") (fileName ==? "package.json") (toFilePath basedir)
  files <- embed @IO $ traverse (stripProperPrefix basedir <=< parseAbsFile) paths
  traverse_ (output . configure . parent) files

strategy :: Strategy NpmOpts
strategy = Strategy
  { strategyName = "nodejs-npm"
  , strategyAnalyze = analyze
  }

analyze :: Members '[Exec, Error CLIErr] r => NpmOpts -> Sem r G.Graph
analyze NpmOpts{..} = do
  (exitcode, stdout, stderr) <- exec npmOptsDir "npm" ["ls", "--json", "--production"]
  when (exitcode /= ExitSuccess) (throw $ StrategyFailed $ "NPM returned an error: " <> stderr ^. unpackedChars)
  case eitherDecode stdout of
    Left err -> throw $ StrategyFailed err -- TODO: better error
    Right a -> pure $ buildGraph a

buildGraph :: NpmOutput -> G.Graph
buildGraph top = unfold direct getDeps toDependency
  where
  direct = M.toList $ outputDependencies top
  getDeps (_,nodeOutput) = M.toList $ outputDependencies nodeOutput
  toDependency (nodeName, nodeOutput) =
    G.Dependency { dependencyType = G.NodeJSType
                 , dependencyName = nodeName
                 , dependencyVersion = outputVersion nodeOutput
                 , dependencyLocations = []
                 }

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . NpmOpts

data NpmOutput = NpmOutput
  { outputVersion      :: Maybe Text
  , outputFrom         :: Maybe Text
  , outputResolved     :: Maybe Text
  , outputDependencies :: Map Text NpmOutput
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON NpmOutput where
  parseJSON = withObject "NpmOutput" $ \obj ->
    NpmOutput <$> obj .:? "version"
              <*> obj .:? "from"
              <*> obj .:? "resolved"
              <*> obj .:? "dependencies" .!= M.empty
