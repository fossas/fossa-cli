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
discover' = walk $ \_ subdirs files -> do
  case find (\f -> fileName f == "project.json") files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkSkipNamed ["node_modules/"] subdirs

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nuget-projectjson"
  , strategyAnalyze = \opts -> analyze
      & fileInputJson @ProjectJson (targetFile opts)
  , strategyModule = parent. targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

data ProjectJson = ProjectJson
  { dependencies     :: Map Text NuGetDependency
  } deriving Show

instance FromJSON ProjectJson where
  parseJSON = withObject "ProjectJson" $ \obj ->
    ProjectJson <$> obj .: "dependencies"

data NuGetDependency = NuGetDependency
  { depVersion    :: Text
  , depType       :: Maybe Text
  } deriving Show

instance FromJSON NuGetDependency where
  parseJSON val = parseJSONObject val <|> parseJSONText val
    where
    parseJSONObject :: Value -> Parser NuGetDependency
    parseJSONObject = withObject "Dependency" $ \obj ->
        NuGetDependency <$> obj .: "version"
                        <*> obj .:? "type"
            
    parseJSONText :: Value -> Parser NuGetDependency
    parseJSONText = withText "NuGetDependencyCustom" $ \text ->
        pure $ NuGetDependency text Nothing


analyze :: Member (Input ProjectJson) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

data CompleteNugetDep = CompleteNugetDep
  { depName            :: Text
  , depVersions        :: Text
  , completeDepType    :: Maybe Text
  } deriving Show

buildGraph :: ProjectJson -> Graphing Dependency
buildGraph project = unfold direct (const []) toDependency
    where
    direct = (\(name, dep) -> CompleteNugetDep name (depVersion dep) (depType dep)) <$> M.toList (dependencies project)
    toDependency CompleteNugetDep{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depName
               , dependencyVersion = case T.find('*' ==) depVersions of
                  Just '*' -> Just (CCompatible depVersions)
                  _ -> Just (CEq depVersions)
               , dependencyLocations = []
               , dependencyTags = case completeDepType of
                  Nothing -> M.empty
                  Just depType -> M.insert "type" [depType]  M.empty
               }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts