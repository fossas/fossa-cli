module Strategy.Bundler (
  discover,
  findProjects,
  mkProject,
  getDeps,
) where

import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Control.Effect.Diagnostics qualified as Diag
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Path
import Strategy.Ruby.BundleShow qualified as BundleShow
import Strategy.Ruby.GemfileLock qualified as GemfileLock
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
              { bundlerGemfile = gemfile
              , bundlerGemfileLock = gemfileLock
              , bundlerDir = dir
              }

      pure ([project], WalkContinue)

data BundlerProject = BundlerProject
  { bundlerGemfile :: Path Abs File
  , bundlerGemfileLock :: Maybe (Path Abs File)
  , bundlerDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Exec sig n, Has Diagnostics sig n) => BundlerProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "bundler"
    , projectBuildTargets = mempty
    , projectDependencyResults = const $ getDeps project
    , projectPath = bundlerDir project
    , projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => BundlerProject -> m DependencyResults
getDeps project = context "Bundler" $ analyzeBundleShow project <||> analyzeGemfileLock project

analyzeBundleShow :: (Has Exec sig m, Has Diagnostics sig m) => BundlerProject -> m DependencyResults
analyzeBundleShow project = do
  graph <- context "bundle-show analysis" . BundleShow.analyze' . bundlerDir $ project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = maybe [bundlerGemfile project] pure (bundlerGemfileLock project)
      }

analyzeGemfileLock :: (Has ReadFS sig m, Has Diagnostics sig m) => BundlerProject -> m DependencyResults
analyzeGemfileLock project = do
  lockFile <- context "Retrieve Gemfile.lock" (Diag.fromMaybeText "No Gemfile.lock present in the project" (bundlerGemfileLock project))
  graph <- context "Gemfile.lock analysis" . GemfileLock.analyze' $ lockFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [lockFile]
      }
