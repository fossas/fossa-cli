module Strategy.NuGet.ProjectJson
  ( discover
  , strategy
  , buildGraph
  , analyze

  , ProjectJson(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import           Data.Aeson.Types

import           DepTypes
import           Discovery.Walk
import           Effect.ReadFS
import           Graphing (Graphing, unfold)
import           Types

discover :: Discover
discover = Discover
  { discoverName = "projectjson"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "project.json") files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nuget-projectjson"
  , strategyAnalyze = \opts -> analyze
      & fileInputJson @ProjectJson (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

data ProjectJson = ProjectJson
  { dependencies     :: Map Text DependencyInfo
  } deriving Show

data DependencyInfo = DependencyInfo
  { depVersion    :: Text
  , depType       :: Maybe Text
  } deriving Show

instance FromJSON ProjectJson where
  parseJSON = withObject "ProjectJson" $ \obj ->
    ProjectJson <$> obj .: "dependencies"

instance FromJSON DependencyInfo where
  parseJSON val = parseJSONObject val <|> parseJSONText val
    where
    parseJSONObject :: Value -> Parser DependencyInfo
    parseJSONObject = withObject "DependencyInfo" $ \obj ->
        DependencyInfo <$> obj .: "version"
                        <*> obj .:? "type"
            
    parseJSONText :: Value -> Parser DependencyInfo
    parseJSONText = withText "DependencyVersion" $ \text ->
        pure $ DependencyInfo text Nothing

analyze :: Member (Input ProjectJson) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

data NuGetDependency = NuGetDependency
  { name            :: Text
  , version         :: Text
  , dependencyType  :: Maybe Text
  } deriving Show

buildGraph :: ProjectJson -> Graphing Dependency
buildGraph project = unfold direct (const []) toDependency
    where
    direct = (\(name, dep) -> NuGetDependency name (depVersion dep) (depType dep)) <$> M.toList (dependencies project)
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = name
               , dependencyVersion = case T.find ('*' ==) version of
                  Just '*' -> Just (CCompatible version)
                  _ -> Just (CEq version)
               , dependencyLocations = []
               , dependencyTags = case dependencyType of
                  Nothing -> M.empty
                  Just depType -> M.insert "type" [depType]  M.empty
               }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
