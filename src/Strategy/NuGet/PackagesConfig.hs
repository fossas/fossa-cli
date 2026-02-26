{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.PackagesConfig (
  discover,
  findProjects,
  getDeps,
  mkProject,
  buildGraph,
  PackagesConfig (..),
  NuGetDependency (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Foldable (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes (
  DepType (NuGetType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  fileName,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS, readContentsXML)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Graphing qualified
import Parse.XML (FromXML (..), attr, children)
import Path (Abs, Dir, File, Path, parent)
import Strategy.NuGet.Util (resolvedVersion)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (PackagesConfigProjectType),
  GraphBreadth (Partial),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject PackagesConfigProject]
discover = simpleDiscover findProjects mkProject PackagesConfigProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [PackagesConfigProject]
findProjects = walkWithFilters' $ \_ _ files -> do
  case find (\f -> fileName f == "packages.config") files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([PackagesConfigProject file], WalkContinue)

newtype PackagesConfigProject = PackagesConfigProject
  { packagesConfigFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PackagesConfigProject

instance AnalyzeProject PackagesConfigProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

mkProject :: PackagesConfigProject -> DiscoveredProject PackagesConfigProject
mkProject project =
  DiscoveredProject
    { projectType = PackagesConfigProjectType
    , projectBuildTargets = mempty
    , projectPath = parent $ packagesConfigFile project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PackagesConfigProject -> m DependencyResults
getDeps = context "PackagesConfig" . context "Static analysis" . analyze' . packagesConfigFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  config <- readContentsXML @PackagesConfig file
  graph <- context "Building dependency graph" $ pure (buildGraph config)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [file]
      }

instance FromXML PackagesConfig where
  parseElement el = PackagesConfig <$> children "package" el

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency
      <$> attr "id" el
      <*> attr "version" el

newtype PackagesConfig = PackagesConfig
  { deps :: [NuGetDependency]
  }
  deriving (Eq, Ord, Show)

data NuGetDependency = NuGetDependency
  { depID :: Text
  , depVersion :: Text
  }
  deriving (Eq, Ord, Show)

buildGraph :: PackagesConfig -> Graphing Dependency
buildGraph = Graphing.fromList . map toDependency . deps
  where
    toDependency NuGetDependency{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = depID
        , dependencyVersion = fmap CEq (resolvedVersion (Just depVersion))
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
