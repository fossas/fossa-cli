module Strategy.Npm
  ( discover
  )
where

import Control.Effect.Diagnostics ((<||>), Diagnostics)
import qualified Control.Effect.Diagnostics as Diag
import Control.Monad.IO.Class
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing
import Path
import qualified Strategy.Node.NpmList as NpmList
import qualified Strategy.Node.NpmLock as NpmLock
import qualified Strategy.Node.PackageJson as PackageJson
import Types

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [NpmProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "package.json" files of
    Nothing -> pure ([], WalkContinue)
    Just packageJson -> do
      let packageLock = findFileNamed "package-lock.json" files

      let project =
            NpmProject
              { npmDir = dir,
                npmPackageJson = packageJson,
                npmPackageLock = packageLock
              }

      pure ([project], WalkSkipAll)

data NpmProject = NpmProject
  { npmDir :: Path Abs Dir,
    npmPackageJson :: Path Abs File,
    npmPackageLock :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show)

mkProject :: NpmProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "npm",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO . runExecIO $ getDeps project,
      projectPath = npmDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m (Graphing Dependency)
--getDeps project = Npm3Tree.analyze' (npmDir project)
getDeps project = analyzeNpmList project <||> analyzeNpmLock project <||> analyzeNpmJson project

analyzeNpmList :: (Has Exec sig m, Has Diagnostics sig m) => NpmProject -> m (Graphing Dependency)
analyzeNpmList = NpmList.analyze' . npmDir

analyzeNpmLock :: (Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m (Graphing Dependency)
analyzeNpmLock project = Diag.fromMaybeText "No package-lock.json present in the project" (npmPackageLock project) >>= NpmLock.analyze'

analyzeNpmJson :: (Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m (Graphing Dependency)
analyzeNpmJson = PackageJson.analyze' . npmPackageJson
