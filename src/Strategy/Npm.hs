{-# language QuasiQuotes #-}

module Strategy.Npm
  ( discover
  , strategy
  , analyze
  , configure
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Types

discover :: Discover
discover = Discover
  { discoverName = "npm-list"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir subdirs files -> do
  for_ files $ \f ->
    when (fileName f == "package.json") $
      output (configure dir)

  walkSkipNamed ["node_modules/"] subdirs

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "nodejs-npm"
  , strategyAnalyze = \opts -> analyze & execInputJson (targetDir opts) npmListCmd
  , strategyModule = targetDir
  }

npmListCmd :: Command
npmListCmd = Command
  { cmdNames = [[relfile|npm|]]
  , cmdArgs = ["ls", "--json", "--production"]
  }

analyze :: Member (Input NpmOutput) r => Sem r G.Graph
analyze = buildGraph <$> input

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
                 , dependencyTags = M.empty -- TODO
                 }

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicDirOpts

data NpmOutput = NpmOutput
  { outputInvalid      :: Maybe Bool
  , outputVersion      :: Maybe Text
  , outputFrom         :: Maybe Text
  , outputResolved     :: Maybe Text
  , outputDependencies :: Map Text NpmOutput
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON NpmOutput where
  parseJSON = withObject "NpmOutput" $ \obj ->
    NpmOutput <$> obj .:? "invalid"
              <*> obj .:? "version"
              <*> obj .:? "from"
              <*> obj .:? "resolved"
              <*> obj .:? "dependencies" .!= M.empty
