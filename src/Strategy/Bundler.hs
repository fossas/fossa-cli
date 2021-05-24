module Strategy.Bundler
  ( discover,
    findProjects,
    mkProject,
    getDeps,
  )
where

import Control.Effect.Diagnostics (Diagnostics, (<||>), context)
import qualified Control.Effect.Diagnostics as Diag
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing
import Path
import qualified Strategy.Ruby.BundleShow as BundleShow
import qualified Strategy.Ruby.GemfileLock as GemfileLock
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Bundler" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [BundlerProject]
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

mkProject :: (Has ReadFS sig n, Has Exec sig n, Has Diagnostics sig n) => BundlerProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "bundler",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = bundlerDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => BundlerProject -> m (Graphing Dependency)
getDeps project = context "Bundler" $ analyzeBundleShow project <||> analyzeGemfileLock project

analyzeBundleShow :: (Has Exec sig m, Has Diagnostics sig m) => BundlerProject -> m (Graphing Dependency)
analyzeBundleShow = context "bundle-show analysis" . BundleShow.analyze' . bundlerDir

analyzeGemfileLock :: (Has ReadFS sig m, Has Diagnostics sig m) => BundlerProject -> m (Graphing Dependency)
analyzeGemfileLock project = context "Gemfile.lock analysis" (Diag.fromMaybeText "No Gemfile.lock present in the project" (bundlerGemfileLock project)) >>= GemfileLock.analyze'
