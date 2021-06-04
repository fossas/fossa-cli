module Strategy.Npm
  ( discover
  )
where

import Control.Effect.Diagnostics ((<||>), Diagnostics, context)
import qualified Control.Effect.Diagnostics as Diag
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing
import Path
import qualified Strategy.Node.NpmList as NpmList
import qualified Strategy.Node.NpmLock as NpmLock
import qualified Strategy.Node.PackageJson as PackageJson
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
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
                  { npmDir = dir,
                    npmPackageJson = packageJson,
                    npmPackageLock = packageLock
                  }

          pure ([project], WalkSkipSome ["node_modules"])

data NpmProject = NpmProject
  { npmDir :: Path Abs Dir,
    npmPackageJson :: Path Abs File,
    npmPackageLock :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Exec sig n, Has Diagnostics sig n) => NpmProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "npm",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = npmDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m (Graphing Dependency)
getDeps project = context "Npm" $ analyzeNpmList project <||> analyzeNpmLock project <||> analyzeNpmJson project

analyzeNpmList :: (Has Exec sig m, Has Diagnostics sig m) => NpmProject -> m (Graphing Dependency)
analyzeNpmList = context "npm-list analysis" . NpmList.analyze' . npmDir

analyzeNpmLock :: (Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m (Graphing Dependency)
analyzeNpmLock project = context "package-lock.json analysis" $
  Diag.fromMaybeText "No package-lock.json present in the project" (npmPackageLock project) >>= NpmLock.analyze'

analyzeNpmJson :: (Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m (Graphing Dependency)
analyzeNpmJson = context "package.json analysis" . PackageJson.analyze' . npmPackageJson
