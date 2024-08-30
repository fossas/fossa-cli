module Strategy.Cocoapods (
  discover,
  findProjects,
  getDeps,
  mkProject,
  CocoapodsProject (..),
) where

import App.Fossa.Analyze.LicenseAnalyze (LicenseAnalyzeProject, licenseAnalyzeProject)
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import App.Types (Mode (..))
import App.Util (guardStrictMode, populateWarningsForAnalysisMode)
import Control.Applicative ((<|>))
import Control.Carrier.Diagnostics (errHelp)
import Control.Effect.Diagnostics (Diagnostics, context, errCtx, errDoc, (<||>))
import Control.Effect.Diagnostics qualified as Diag
import Control.Effect.Reader (Reader, ask)
import Data.Aeson (ToJSON)
import Data.Glob qualified as Glob
import Data.List (find)
import Data.List.Extra (singleton)
import Data.Text (isSuffixOf)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  findFilesMatchingGlob,
  walkWithFilters',
 )
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (Has, ReadFS, readContentsParser)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, toFilePath)
import Strategy.Cocoapods.Errors (MissingPodLockFile (..), refPodDocUrl)
import Strategy.Cocoapods.Podfile qualified as Podfile
import Strategy.Cocoapods.PodfileLock qualified as PodfileLock
import Strategy.Ruby.Parse (Assignment (label, value), PodSpecAssignmentValue (PodspecDict, PodspecStr), Symbol (Symbol), findBySymbol, podspecAssignmentValuesP, readAssignments)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (CocoapodsProjectType),
  GraphBreadth (Complete, Partial),
  License (License),
  LicenseResult (LicenseResult, licenseFile, licensesFound),
  LicenseType (UnknownType),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject CocoapodsProject]
discover = simpleDiscover findProjects mkProject CocoapodsProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [CocoapodsProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let podfile = findFileNamed "Podfile" files
      podfileLock = findFileNamed "Podfile.lock" files
      podSpecs = findFilesMatchingGlob (Glob.toGlob dir Glob.</> "*.podspec") files

  let project =
        CocoapodsProject
          { cocoapodsPodfile = podfile
          , cocoapodsPodfileLock = podfileLock
          , cocoapodsDir = dir
          , cocoapodsSpecFiles = podSpecs
          }

  case podfile <|> podfileLock of
    Nothing -> pure ([], WalkContinue)
    Just _ -> pure ([project], WalkContinue)

data CocoapodsProject = CocoapodsProject
  { cocoapodsPodfile :: Maybe (Path Abs File)
  , cocoapodsPodfileLock :: Maybe (Path Abs File)
  , cocoapodsDir :: Path Abs Dir
  , cocoapodsSpecFiles :: [Path Abs File]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CocoapodsProject

instance AnalyzeProject CocoapodsProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps'

instance LicenseAnalyzeProject CocoapodsProject where
  licenseAnalyzeProject = traverse readLicense . cocoapodsSpecFiles

-- | For now, if the 'license' assignment statement is a dictionary this
--  code only extracts the value in the `:type` key. It also only looks at
--  the first `license` assignment it finds in the spec file.
readLicense :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m LicenseResult
readLicense specFile = do
  assignments <- readContentsParser (readAssignments podspecAssignmentValuesP) specFile
  let maybeLicense = do
        licenseAssignment <- value <$> find (("license" `isSuffixOf`) . label) assignments
        case licenseAssignment of
          PodspecStr s -> Just $ License UnknownType s
          PodspecDict d -> Just . License UnknownType . snd =<< findBySymbol (Symbol "type") d
  pure $
    LicenseResult
      { licenseFile = toFilePath specFile
      , licensesFound = maybe [] singleton maybeLicense
      }

mkProject :: CocoapodsProject -> DiscoveredProject CocoapodsProject
mkProject project =
  DiscoveredProject
    { projectType = CocoapodsProjectType
    , projectBuildTargets = mempty
    , projectPath = cocoapodsDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m, Has Exec sig m, Has Logger sig m, Has (Reader Mode) sig m) => CocoapodsProject -> m DependencyResults
getDeps project = do
  mode <- ask
  context "Cocoapods" $
    context
      "Podfile.lock analysis"
      ( populateWarningsForAnalysisMode mode
          . errCtx MissingPodLockFileCtx
          . errHelp MissingPodLockFileHelp
          . errDoc refPodDocUrl
          $ (analyzePodfileLock project)
      )
      <||> guardStrictMode mode (context "Podfile analysis" (analyzePodfile project))

getDeps' :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader Mode) sig m) => CocoapodsProject -> m DependencyResults
getDeps' project = do
  mode <- ask
  context "Cocoapods" $
    context
      "Podfile.lock analysis"
      ( populateWarningsForAnalysisMode mode
          . errCtx MissingPodLockFileCtx
          . errHelp MissingPodLockFileHelp
          . errDoc refPodDocUrl
          $ (analyzePodfileLockStatically project)
      )
      <||> guardStrictMode mode (context "Podfile analysis" (analyzePodfile project))

analyzePodfile :: (Has ReadFS sig m, Has Diagnostics sig m) => CocoapodsProject -> m DependencyResults
analyzePodfile project = do
  podFile <- Diag.fromMaybeText "No Podfile present in the project" (cocoapodsPodfile project)
  graph <- Podfile.analyze' podFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [podFile]
      }

analyzePodfileLock :: (Has ReadFS sig m, Has Diagnostics sig m, Has Exec sig m, Has Logger sig m) => CocoapodsProject -> m DependencyResults
analyzePodfileLock project = do
  lockFile <- Diag.fromMaybeText "No Podfile.lock present in the project" (cocoapodsPodfileLock project)
  graph <- PodfileLock.analyze' lockFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [lockFile]
      }

analyzePodfileLockStatically :: (Has ReadFS sig m, Has Diagnostics sig m) => CocoapodsProject -> m DependencyResults
analyzePodfileLockStatically project = do
  lockFile <- Diag.fromMaybeText "No Podfile.lock present in the project" (cocoapodsPodfileLock project)
  graph <- PodfileLock.analyzeStatic lockFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [lockFile]
      }
