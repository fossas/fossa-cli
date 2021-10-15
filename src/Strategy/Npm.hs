module Strategy.Npm (
  discover,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Control.Effect.Diagnostics qualified as Diag
import Data.Aeson (ToJSON)
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import GHC.Generics (Generic)
import Path
import Strategy.Node.NpmList qualified as NpmList
import Strategy.Node.NpmLock qualified as NpmLock
import Strategy.Node.PackageJson qualified as PackageJson
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject NpmProject]
discover dir = context "Npm" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [NpmProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "yarn.lock" files of
    -- When we find yarn.lock, assume this directory and subdirectories are managed by yarn.
    -- This prevents duplicate project analysis and noisy failures
    Just _ -> pure ([], WalkSkipAll)
    Nothing ->
      case findFileNamed "package.json" files of
        Nothing -> pure ([], WalkSkipSome ["node_modules"])
        Just packageJson -> do
          let packageLock = findFileNamed "package-lock.json" files

          let project =
                NpmProject
                  { npmDir = dir
                  , npmPackageJson = packageJson
                  , npmPackageLock = packageLock
                  }

          pure ([project], WalkSkipSome ["node_modules"])

data NpmProject = NpmProject
  { npmDir :: Path Abs Dir
  , npmPackageJson :: Path Abs File
  , npmPackageLock :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NpmProject

instance AnalyzeProject NpmProject where
  analyzeProject _ = getDeps

mkProject :: NpmProject -> DiscoveredProject NpmProject
mkProject project =
  DiscoveredProject
    { projectType = "npm"
    , projectBuildTargets = mempty
    , projectPath = npmDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m DependencyResults
getDeps project = context "Npm" $ analyzeNpmList project <||> analyzeNpmLock project <||> analyzeNpmJson project

analyzeNpmList :: (Has Exec sig m, Has Diagnostics sig m) => NpmProject -> m DependencyResults
analyzeNpmList project = do
  graph <- context "npm-list analysis" . NpmList.analyze' $ npmDir project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = maybe [npmPackageJson project] pure $ npmPackageLock project
      }

analyzeNpmLock :: (Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m DependencyResults
analyzeNpmLock project = do
  lockFile <- Diag.fromMaybeText "No package-lock.json present in the project" (npmPackageLock project)
  graph <- context "package-lock.json analysis" . NpmLock.analyze' $ lockFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [lockFile]
      }

analyzeNpmJson :: (Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m DependencyResults
analyzeNpmJson project = do
  graph <- context "package.json analysis" . PackageJson.analyze' $ npmPackageJson project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [npmPackageJson project]
      }
