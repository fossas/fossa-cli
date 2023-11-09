{-# LANGUAGE TemplateHaskell #-}

-- Strategy.Gradle provides dependency analysis for Gradle projects. It works by
-- looking for `build.gradle` files and shelling out to `gradle` using a bundled
-- Gradle init script (`jsondeps.gradle`). The init script does most of the
-- thinking, and the analyzer just invokes it and parses the JSON output.
--
-- If you're debugging this analyzer, you'll want to start by getting the output
-- of invoking the Gradle script. You should be able to find this output from
-- the replay logs of the analysis.
--
-- Useful links:
-- - Gradle subprojects: https://docs.gradle.org/current/userguide/multi_project_builds.html
-- - Gradle tasks: https://docs.gradle.org/current/userguide/tutorial_using_tasks.html
-- - Gradle init scripts: https://docs.gradle.org/current/userguide/init_scripts.html
module Strategy.Gradle (
  discover,
  GradleProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig (allowedGradleConfigs))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  errCtx,
  fatalText,
  recover,
  warnOnErr,
  (<||>),
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.Reader (Reader, asks)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed.Extra (embedFile')
import Data.Foldable (find, traverse_)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty, toSet)
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (traceM)
import DepTypes (
  Dependency (..),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (WalkStep (..), fileName, findFileInAncestor, walkWithFilters')
import Effect.Exec (AllowErr (..), Command (..), Exec, execThrow)
import Effect.Logger (Logger, Pretty (pretty), logDebug)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, fromAbsDir)
import Strategy.Gradle.Common (
  ConfigName (..),
  getDebugMessages,
 )
import Strategy.Gradle.Errors (FailedToListProjects (FailedToListProjects), FailedToRunGradleAnalysis (FailedToRunGradleAnalysis), GradleWrapperFailed (GradleWrapperFailed))
import Strategy.Gradle.ResolutionApi qualified as ResolutionApi
import System.FilePath qualified as FilePath
import Types (BuildTarget (..), DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (GradleProjectType), FoundTargets (..), GraphBreadth (..))

-- | Run the init script on a set of subprojects. Note that this runs the
--  `:jsonDeps` task on every subproject in one command. This is helpful for
--  performance reasons, because Gradle has a slow startup on each invocation.
gradleJsonDepsCmdTargets :: FilePath -> Set BuildTarget -> Text -> Command
gradleJsonDepsCmdTargets initScriptFilepath targets baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["-I", toText initScriptFilepath] ++ map (\target -> unBuildTarget target <> ":jsonDeps") (Set.toList targets)
    , cmdAllowErr = Never
    }

-- | Run the init script on a root project.
gradleJsonDepsCmd :: FilePath -> Text -> Command
gradleJsonDepsCmd initScriptFilepath baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["-I", toText initScriptFilepath, "jsonDeps"]
    , cmdAllowErr = Never
    }

discover ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject GradleProject]
discover = simpleDiscover findProjects mkProject GradleProjectType

-- | Run a Gradle command in a specific working directory, while correctly trying
-- Gradle wrappers.
runGradle :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> (Text -> Command) -> m BL.ByteString
runGradle dir cmd = gradleWrapper <||> gradleBinary
  where
    gradleBinary :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => m BL.ByteString
    gradleBinary = execThrow dir (cmd "gradle")

    gradleWrapper :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => m BL.ByteString
    gradleWrapper = warnOnErr GradleWrapperFailed $ (findFileInAncestor dir "gradlew" >>= execThrow dir . cmd . toText) <||> (findFileInAncestor dir "gradlew.bat" >>= execThrow dir . cmd . toText)

-- Search for a `build.gradle`. Each `build.gradle` is its own analysis target.
--
-- In the user documentation, we say that each Gradle subproject is its own
-- analysis target. This is morally true, but in our implementation we treat
-- each `build.gradle` as an analysis target.
--
-- This is to avoid invoking Gradle again for each subproject, which would be
-- slow (because of Gradle's startup time) and possibly wrong (because
-- subprojects need to resolve dependency constraints together).
findProjects :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [GradleProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let isProjectFile f =
        any
          (`isPrefixOf` fileName f)
          [ "build.gradle"
          , "settings.gradle"
          ]

  case find isProjectFile files of
    Nothing -> pure ([], WalkContinue)
    Just buildFile -> do
      projectsStdout <-
        recover
          . warnOnErr (FailedToListProjects dir)
          . context ("Listing gradle projects at '" <> toText dir <> "'")
          $ runGradle dir gradleProjectsCmd

      case projectsStdout of
        -- Nearly all Gradle projects have a multi-module structure with a
        -- top-level root project, and subdirectories contain subprojects of
        -- the top-level root project.
        --
        -- If Gradle exits non-zero when invoked in the root project, it will
        -- almost certainly exit non-zero in subprojects, so there's no point
        -- in looking for subprojects in this subtree.
        Nothing -> pure ([], WalkSkipAll)
        Just result -> do
          let subprojects = parseProjects result

          let project =
                GradleProject
                  { gradleDir = dir
                  , gradleBuildFile = buildFile
                  , gradleProjects = subprojects
                  }

          pure ([project], WalkSkipAll)

data GradleProject = GradleProject
  { gradleDir :: Path Abs Dir
  , gradleBuildFile :: Path Abs File
  , gradleProjects :: Set Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GradleProject

instance AnalyzeProject GradleProject where
  analyzeProject = getDeps
  analyzeProject' _ = const $ fatalText "Cannot analyze Gradle target statically"

gradleProjectsCmd :: Text -> Command
gradleProjectsCmd baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["projects"]
    , cmdAllowErr = Never
    }

-- We use a single empty-string target when no subprojects exist. Gradle uses an
-- empty string to denote the root project when invoking tasks, e.g., ":task"
-- instead of ":subproject:task".
parseProjects :: BL.ByteString -> Set Text
parseProjects outBL = if Set.null subprojects then Set.singleton "" else subprojects
  where
    subprojects = Set.fromList $ mapMaybe parseSubproject outLines

    outText = decodeUtf8 $ BL.toStrict outBL
    outLines = Text.lines outText

-- | Parse a subproject line from the gradle output, e.g.,
--
-- >>> parseSubproject "+--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "    +--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "\\--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "|   +--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "anyprefix +--- Project ':foo'"
-- Just ":foo"
--
-- >>> parseSubproject "anything else"
-- Nothing
parseSubproject :: Text -> Maybe Text
parseSubproject line =
  case Text.breakOnEnd "--- Project '" (Text.strip line) of
    ("", _) -> Nothing -- no match
    (_, rest) -> Just $ Text.takeWhile (/= '\'') rest

mkProject :: GradleProject -> DiscoveredProject GradleProject
mkProject project =
  DiscoveredProject
    { projectType = GradleProjectType
    , projectBuildTargets = maybe ProjectWithoutTargets FoundTargets $ nonEmpty $ Set.map BuildTarget $ gradleProjects project
    , projectPath = gradleDir project
    , projectData = project
    }

getDeps ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  ) =>
  FoundTargets ->
  GradleProject ->
  m DependencyResults
getDeps targets project = context "Gradle" $ do
  traceM ("Targets in getDeps Gradle.hs ----- " ++ show (targets))
  graph <- analyze targets (gradleDir project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [gradleBuildFile project]
      }

-- See the release process to see how this script gets vendored.
initScript :: ByteString
initScript = $(embedFile' "scripts/jsondeps.gradle")

-- During analysis, we unpack the Gradle init script, run it, and parse the
-- output.
analyze ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  ) =>
  FoundTargets ->
  Path Abs Dir ->
  m (Graphing Dependency)
analyze foundTargets dir = withSystemTempDir "fossa-gradle" $ \tmpDir -> do
  let initScriptFilepath = fromAbsDir tmpDir FilePath.</> "jsondeps.gradle"
  context "Writing gradle script" $ sendIO (BS.writeFile initScriptFilepath initScript)

  let cmd :: Text -> Command
      cmd = case foundTargets of
        FoundTargets targets -> gradleJsonDepsCmdTargets initScriptFilepath (toSet targets)
        ProjectWithoutTargets -> gradleJsonDepsCmd initScriptFilepath

  stdout <- context "running gradle script" $ errCtx FailedToRunGradleAnalysis $ runGradle dir cmd

  onlyConfigurations <- do
    configs <- asks allowedGradleConfigs
    pure $ maybe Set.empty (Set.map ConfigName) configs

  let text = decodeUtf8 $ BL.toStrict stdout
  let resolvedProjects = ResolutionApi.parseResolutionApiJsonDeps text
  let graphFromResolutionApi = ResolutionApi.buildGraph resolvedProjects (onlyConfigurations)

  -- Log debug messages as seen in gradle script
  traverse_ (logDebug . pretty) (getDebugMessages text)

  context "Building dependency graph" $ pure graphFromResolutionApi
