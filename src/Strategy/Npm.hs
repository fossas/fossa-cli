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
import           Discovery.Core
import           Discovery.Walk
import           Effect.ErrorTrace
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy

discover :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover = walk $ \dir subdirs files -> do
  for_ files $ \f -> do
    when (fileName f == "package.json") $
      output (configure dir)

  walkSkipNamed ["node_modules"] subdirs

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "nodejs-npm"
  , strategyAnalyze = analyze
  }

analyze :: Members '[Exec, Error CLIErr] r => BasicDirOpts -> Sem r G.Graph
analyze BasicDirOpts{..} = do
  (exitcode, stdout, stderr) <- exec targetDir "npm" ["ls", "--json", "--production"]
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
configure = ConfiguredStrategy strategy . BasicDirOpts

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
