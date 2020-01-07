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
discover' = walk $ \_ subdirs files -> do
  case find (\f -> fileName f == "project.assets.json") files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkSkipNamed ["node_modules/"] subdirs

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
  { targets     :: M.Map Text (M.Map Text NugetDep)
  } deriving Show

instance FromJSON ProjectAssetsJson where
  parseJSON = withObject "ProjectAssetsJson" $ \obj ->
    ProjectAssetsJson <$> obj .: "targets"

data NugetDep = NugetDep
  { depType    :: Text
  , deepDeps   :: Maybe (M.Map Text Text)
  } deriving Show
   
data CompleteNugetDep = CompleteNugetDep
  { depName            :: Text
  , depVersion         :: Text
  , completeDepType    :: Text
  , completeDeepDeps   :: Maybe (M.Map Text Text)
  } deriving Show

instance FromJSON NugetDep where
  parseJSON = withObject "Dependency" $ \obj ->
    NugetDep <$> obj .: "type"
             <*> obj .:? "dependencies"

analyze :: Member (Input ProjectAssetsJson) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

buildGraph :: ProjectAssetsJson -> Graphing Dependency
buildGraph project = unfold direct deepList toDependency
    where
    direct = concat $ (\(_,a) -> ((\(x,y) -> CompleteNugetDep (head $ T.splitOn "/" x) (last $ T.splitOn "/" x) (depType y) (deepDeps y)) <$> (M.toList a))) <$> (M.toList (targets project))
    deepList nugetDep = (\(x,y) -> CompleteNugetDep x y "" Nothing) <$>
      case completeDeepDeps nugetDep of 
        Nothing -> []
        Just dependencies -> M.toList dependencies
    toDependency CompleteNugetDep{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
