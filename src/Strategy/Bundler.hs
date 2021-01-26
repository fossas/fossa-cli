module Strategy.Bundler
  ( discover,
    findProjects,
    mkProject,
    getDeps,
  )
where

import Control.Effect.Diagnostics (Diagnostics, (<||>))
import qualified Control.Effect.Diagnostics as Diag
import Control.Monad.IO.Class
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing
import Path
import qualified Strategy.Ruby.BundleShow as BundleShow
import qualified Strategy.Ruby.GemfileLock as GemfileLock
import Types

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [BundlerProject]
findProjects = walk' $ \dir _ files -> do
  let maybeGemfile = findFileNamed "Gemfile" files
  let gemfileLock = findFileNamed "Gemfile.lock" files

  case maybeGemfile of
    Nothing -> pure ([], WalkContinue)
    Just gemfile -> do
      let project =
            BundlerProject
              { bundlerGemfile = gemfile,
                bundlerGemfileLock = gemfileLock,
                bundlerDir = dir
              }

      pure ([project], WalkContinue)

data BundlerProject = BundlerProject
  { bundlerGemfile :: Path Abs File,
    bundlerGemfileLock :: Maybe (Path Abs File),
    bundlerDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: BundlerProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "bundler",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runExecIO . runReadFSIO $ getDeps project,
      projectPath = bundlerDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => BundlerProject -> m (Graphing Dependency)
getDeps project = analyzeBundleShow project <||> analyzeGemfileLock project

analyzeBundleShow :: (Has Exec sig m, Has Diagnostics sig m) => BundlerProject -> m (Graphing Dependency)
analyzeBundleShow = BundleShow.analyze' . bundlerDir

analyzeGemfileLock :: (Has ReadFS sig m, Has Diagnostics sig m) => BundlerProject -> m (Graphing Dependency)
analyzeGemfileLock project = Diag.fromMaybeText "No Gemfile.lock present in the project" (bundlerGemfileLock project) >>= GemfileLock.analyze'
