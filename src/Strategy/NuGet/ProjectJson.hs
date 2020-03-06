module Strategy.NuGet.ProjectJson
  ( discover
  , buildGraph
  , analyze

  , ProjectJson(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Aeson.Types

import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "project.json") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "nuget-projectjson" DotnetGroup $ analyze file

  walkContinue

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

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsJson @ProjectJson file

mkProjectClosure :: Path Rel File -> ProjectJson -> ProjectClosureBody
mkProjectClosure file projectJson = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph projectJson
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

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
