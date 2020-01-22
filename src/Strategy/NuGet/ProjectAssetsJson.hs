module Strategy.NuGet.ProjectAssetsJson
  ( discover
  , strategy
  , buildGraph
  , analyze

  , ProjectAssetsJson(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Maybe
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           DepTypes
import           Discovery.Walk
import           Effect.ReadFS
import           Graphing (Graphing, unfold)
import           Types

discover :: Discover
discover = Discover
  { discoverName = "projectassetsjson"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "project.assets.json") files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nuget-projectassetsjson"
  , strategyAnalyze = \opts -> analyze
      & fileInputJson @ProjectAssetsJson (targetFile opts)
  , strategyModule = parent. targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

data ProjectAssetsJson = ProjectAssetsJson
  { targets     :: M.Map Text (M.Map Text DependencyInfo)
  } deriving Show

instance FromJSON ProjectAssetsJson where
  parseJSON = withObject "ProjectAssetsJson" $ \obj ->
    ProjectAssetsJson <$> obj .: "targets"

data DependencyInfo = DependencyInfo
  { depType    :: Text
  , deepDeps   :: M.Map Text Text
  } deriving Show
   
instance FromJSON DependencyInfo where
  parseJSON = withObject "Dependency" $ \obj ->
    DependencyInfo <$> obj .: "type"
             <*> obj .:? "dependencies" .!= M.empty

analyze :: Member (Input ProjectAssetsJson) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

data NuGetDep = NuGetDep
  { depName            :: Text
  , depVersion         :: Text
  , completeDepType    :: Text
  , completeDeepDeps   :: M.Map Text Text
  } deriving Show

buildGraph :: ProjectAssetsJson -> Graphing Dependency
buildGraph project = unfold direct deepList toDependency
    where
    direct :: [NuGetDep] 
    direct = concatMap (mapMaybe convertDep . M.toList) (M.elems (targets project))
                    
    convertDep :: (Text, DependencyInfo) -> Maybe NuGetDep
    convertDep (depString, dep) = case T.splitOn "/" depString of
                  [name, ver] -> Just $ NuGetDep name ver (depType dep) (deepDeps dep) 
                  _ -> Nothing

    deepList nugetDep = (\(x,y) -> NuGetDep x y "" M.empty) <$> (M.toList $ completeDeepDeps nugetDep)
    toDependency NuGetDep{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
