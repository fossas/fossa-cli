module Strategy.Npm
  ( discover
  , strategy
  , analyze
  , configure
  ) where

import Prologue

import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output

import           Config
import           Discovery.Walk
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

discover :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover = walk $ \dir subdirs files -> do
  for_ files $ \f -> do
    when (fileName f == "package.json") $
      output (configure dir)

  walkSkipNamed ["node_modules"] subdirs

strategy :: Strategy NpmOpts
strategy = Strategy
  { strategyName = "nodejs-npm"
  , strategyAnalyze = analyze
  }

analyze :: Members '[Exec, Error CLIErr] r => NpmOpts -> Sem r G.Graph
analyze NpmOpts{..} = do
  (exitcode, stdout, stderr) <- exec npmOptsDir "npm" ["ls", "--json", "--production"]
  when (exitcode /= ExitSuccess) (throw $ StrategyFailed $ "NPM returned an error: " <> BL8.unpack stderr)
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
