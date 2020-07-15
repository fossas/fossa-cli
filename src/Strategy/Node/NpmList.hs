{-# language TemplateHaskell #-}

module Strategy.Node.NpmList
  ( discover
  , analyze
  ) where

import Prologue

import Control.Effect.Diagnostics
import qualified Data.Map.Strict as M
import DepTypes
import Discovery.Walk
import Effect.Exec
import Graphing (Graphing, unfold)
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files -> do
  case find (\f -> fileName f == "package.json") files of
    Nothing -> pure ()
    Just _ -> runSimpleStrategy "nodejs-npmlist" NodejsGroup $ analyze dir

  pure $ WalkSkipSome ["node_modules"]

npmListCmd :: Command
npmListCmd = Command
  { cmdName = "npm"
  , cmdArgs = ["ls", "--json", "--production"]
  , cmdAllowErr = NonEmptyStdout
  }

analyze :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m ProjectClosureBody
analyze dir = mkProjectClosure dir <$> execJson @NpmOutput dir npmListCmd

mkProjectClosure :: Path Abs Dir -> NpmOutput -> ProjectClosureBody
mkProjectClosure dir npmOutput = ProjectClosureBody
  { bodyModuleDir     = dir
  , bodyDependencies  = dependencies
  , bodyLicenses      = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph npmOutput
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

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
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }

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
