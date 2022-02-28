module Strategy.Bundler (
  discover,
  findProjects,
  findLicenses,
  mkProject,
  getDeps,
  genGemspecFilename,
  BundlerProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  errCtx,
  warnOnErr,
  (<||>),
 )
import Control.Effect.Diagnostics qualified as Diag
import Data.Aeson (ToJSON)
import Diag.Common (AllDirectDeps (AllDirectDeps), MissingEdges (MissingEdges))
import Data.Text (isSuffixOf)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walk',
 )
import Effect.Exec (Exec, Has)
import Effect.ReadFS (ReadFS, readContentsParser)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, dirname, fromRelDir, toFilePath)
import Strategy.Ruby.BundleShow qualified as BundleShow
import Strategy.Ruby.Errors (
  BundlerMissingLockFile (..),
 )
import Strategy.Ruby.GemfileLock qualified as GemfileLock
import Strategy.Ruby.Gemspec (Assignment (Assignment, label, value), readAssignments, rubyLicenseValuesP)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (BundlerProjectType),
  GraphBreadth (Complete),
  License (License),
  LicenseResult (LicenseResult),
  LicenseType (UnknownType),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject BundlerProject]
discover dir = context "Bundler" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

genGemspecFilename :: Path Abs Dir -> FilePath
genGemspecFilename dir =
  takeWhile (\c -> c /= '/' && c /= '\\') (fromRelDir (dirname dir)) <> ".gemspec"

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [BundlerProject]
findProjects = walk' $ \dir _ files -> do
  let maybeGemfile = findFileNamed "Gemfile" files
      gemfileLock = findFileNamed "Gemfile.lock" files
      -- TO REVIEW: appending .gemspec to the directory is a bit of a heuristic,
      -- but it's what bundler does by default when making a new
      -- gem. The alternative would be to read the 'name' field.
      gemSpecFile = findFileNamed (genGemspecFilename dir) files

  case maybeGemfile of
    Nothing -> pure ([], WalkContinue)
    Just gemfile -> do
      let project =
            BundlerProject
              { bundlerGemfile = gemfile
              , bundlerGemfileLock = gemfileLock
              , bundlerDir = dir
              , bundlerGemSpec = gemSpecFile
              }

      pure ([project], WalkContinue)

data BundlerProject = BundlerProject
  { bundlerGemfile :: Path Abs File
  , bundlerGemfileLock :: Maybe (Path Abs File)
  , bundlerDir :: Path Abs Dir
  , bundlerGemSpec :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON BundlerProject

instance AnalyzeProject BundlerProject where
  analyzeProject _ = getDeps

instance LicenseAnalyzeProject BundlerProject where
  licenseAnalyzeProject = maybe (pure []) findLicenses . bundlerGemSpec

findLicenses :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m [LicenseResult]
findLicenses gemspecPath = do
  assignments <- readContentsParser (readAssignments rubyLicenseValuesP) gemspecPath
  let licenses = foldMap value . filter isLicenseKey $ assignments

  -- license keys are recommended to be SPDX, but there isn't any requirement:
  -- https://guides.rubygems.org/specification-reference/#license=
  pure [LicenseResult gemSpecFp (License UnknownType <$> licenses)]
  where
    isLicenseKey Assignment{label = label} =
      "license" `isSuffixOf` label
        || "licenses" `isSuffixOf` label
    gemSpecFp = toFilePath gemspecPath

mkProject :: BundlerProject -> DiscoveredProject BundlerProject
mkProject project =
  DiscoveredProject
    { projectType = BundlerProjectType
    , projectBuildTargets = mempty
    , projectPath = bundlerDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => BundlerProject -> m DependencyResults
getDeps project = analyzeGemfileLock project <||> context "Bundler" (analyzeBundleShow project)

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
analyzeGemfileLock project =
  warnOnErr AllDirectDeps
    . warnOnErr MissingEdges
    . errCtx (BundlerMissingLockFile $ bundlerGemfile project)
    $ do
      lockFile <- context "Retrieve Gemfile.lock" (Diag.fromMaybeText "No Gemfile.lock present in the project" (bundlerGemfileLock project))
      graph <- context "Gemfile.lock analysis" . GemfileLock.analyze' $ lockFile
      pure $
        DependencyResults
          { dependencyGraph = graph
          , dependencyGraphBreadth = Complete
          , dependencyManifestFiles = [lockFile]
          }
