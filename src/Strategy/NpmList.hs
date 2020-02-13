module Strategy.NpmList
  ( discover
  , strategy
  , analyze
  , configure
  ) where

import Prologue

import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Input
import Polysemy.Output

import DepTypes
import Discovery.Walk
import Effect.Exec
import Graphing (Graphing, unfold)
import Types

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
  , strategyAnalyze = \opts -> analyze & execInputJson @NpmOutput (targetDir opts) npmListCmd []
  , strategyLicense = const (pure [])
  , strategyModule = targetDir
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

npmListCmd :: Command
npmListCmd = Command
  { cmdNames = ["npm"]
  , cmdBaseArgs = ["ls", "--json", "--production"]
  , cmdAllowErr = NonEmptyStdout
  }

analyze :: Member (Input NpmOutput) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

buildGraph :: NpmOutput -> Graphing Dependency
buildGraph top = unfold direct getDeps toDependency
  where
  direct = M.toList $ outputDependencies top
  getDeps (_,nodeOutput) = M.toList $ outputDependencies nodeOutput
  toDependency (nodeName, nodeOutput) =
    Dependency { dependencyType = NodeJSType
               , dependencyName = nodeName
               , dependencyVersion = CEq <$> outputVersion nodeOutput
               , dependencyLocations = []
               , dependencyTags = M.empty
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
