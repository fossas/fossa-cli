{-# LANGUAGE RecordWildCards #-}

module Strategy.Conan (
  discover,
)
where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Applicative
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic (..),
  fatalText,
 )
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (GetDepsEffs)
import Effect.ReadFS (ReadFS)
import Errata (Errata (..))
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Conan.ConanGraph (analyzeFromConanGraph)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (ConanProjectType),
  GraphBreadth (Complete),
 )

data DynamicAnalysisFailed = DynamicAnalysisFailed

instance ToDiagnostic DynamicAnalysisFailed where
  renderDiagnostic DynamicAnalysisFailed =
    Errata (Just "Dynamic analysis via 'conan graph info' failed") [] Nothing

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject ConanProject]
discover = simpleDiscover findProjects mkProject ConanProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [ConanProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let conanfilePy = findFileNamed "conanfile.py" files
  let conanfileTxt = findFileNamed "conanfile.txt" files
  let conanLock = findFileNamed "conan.lock" files

  let project =
        ConanProject
          { conanDir = dir
          , conanfilePy = conanfilePy
          , conanfileTxt = conanfileTxt
          , conanLock = conanLock
          }

  case conanfilePy <|> conanfileTxt of
    Nothing -> pure ([], WalkSkipAll)
    Just _ -> pure ([project], WalkSkipAll)

data ConanProject = ConanProject
  { conanDir :: Path Abs Dir
  , conanfilePy :: Maybe (Path Abs File)
  , conanfileTxt :: Maybe (Path Abs File)
  , conanLock :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ConanProject

instance AnalyzeProject ConanProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = const $ fatalText "Cannot analyze Conan project statically."

mkProject :: ConanProject -> DiscoveredProject ConanProject
mkProject project =
  DiscoveredProject
    { projectType = ConanProjectType
    , projectBuildTargets = mempty
    , projectPath = conanDir project
    , projectData = project
    }

getDeps :: (GetDepsEffs sig m) => ConanProject -> m DependencyResults
getDeps ConanProject{..} = do
  graph <- analyzeFromConanGraph conanDir
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = catMaybes [conanLock, conanfilePy, conanfileTxt]
      }
