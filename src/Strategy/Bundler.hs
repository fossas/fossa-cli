module Strategy.Bundler (
  discover,
  findProjects,
  findLicenses,
  mkProject,
  getDeps,
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
import Data.Glob as Glob (toGlob, (</>))
import Data.Text (isSuffixOf)
import Diag.Common (AllDirectDeps (AllDirectDeps), MissingEdges (MissingEdges))
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  findFilesMatchingGlob,
  walk',
 )
import Effect.Exec (Exec, Has)
import Effect.ReadFS (ReadFS, readContentsParser)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, toFilePath)
import Strategy.Ruby.BundleShow qualified as BundleShow
import Strategy.Ruby.Errors (
  BundlerMissingLockFile (..),
 )
import Strategy.Ruby.GemfileLock qualified as GemfileLock
import Strategy.Ruby.Parse (Assignment (Assignment, label, value), gemspecLicenseValuesP, readAssignments)
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

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [BundlerProject]
findProjects = walk' $ \dir _ files -> do
  let maybeGemfile = findFileNamed "Gemfile" files
      gemfileLock = findFileNamed "Gemfile.lock" files
      -- Bundler globs for *.gemspec files, so collect all of them for analysis.
      gemSpecFiles = findFilesMatchingGlob (Glob.toGlob dir </> "*.gemspec") files

  case maybeGemfile of
    Nothing -> pure ([], WalkContinue)
    Just gemfile -> do
      let project =
            BundlerProject
              { bundlerGemfile = gemfile
              , bundlerGemfileLock = gemfileLock
              , bundlerDir = dir
              , bundlerGemSpec = gemSpecFiles
              }

      pure ([project], WalkContinue)

data BundlerProject = BundlerProject
  { bundlerGemfile :: Path Abs File
  , bundlerGemfileLock :: Maybe (Path Abs File)
  , bundlerDir :: Path Abs Dir
  , bundlerGemSpec :: [Path Abs File]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON BundlerProject

instance AnalyzeProject BundlerProject where
  analyzeProject _ = getDeps

instance LicenseAnalyzeProject BundlerProject where
  licenseAnalyzeProject = fmap mconcat . traverse findLicenses . bundlerGemSpec

findLicenses :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m [LicenseResult]
findLicenses gemspecPath = do
  assignments <- readContentsParser (readAssignments gemspecLicenseValuesP) gemspecPath
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
