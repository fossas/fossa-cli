{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Reachability.Gradle (gradleJarCallGraph, jarFileSignifier, jarPathsFromScriptOutput) where

import App.Fossa.Reachability.Jar (callGraphFromJar, isValidJar)
import App.Fossa.Reachability.Types (CallGraphAnalysis (..))
import App.Support (reportDefectWithDebugBundle)
import Control.Carrier.Lift (Lift)
import Control.Carrier.Reader (Reader, runReader)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, errCtx, errHelp, errSupport, renderDiagnostic)
import Control.Effect.Lift (sendIO)
import Control.Effect.Path (withSystemTempDir)
import Control.Monad.List (filterM)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Error (createErrataWithHeaderOnly)
import Data.FileEmbed.Extra (embedFile')
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set, toList)
import Data.Set.NonEmpty (toSet)
import Data.String.Conversion (ToString (..), ToText (toText), decodeUtf8)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters)
import Effect.Exec (AllowErr (..), Command (..), Exec)
import Effect.Logger (Logger, logDebug, pretty, renderIt)
import Effect.ReadFS (Has, ReadFS)
import Errata (Errata)
import Path (Abs, Dir, File, Path, fromAbsDir, parseAbsFile)
import Prettyprinter (indent, vsep)
import Strategy.Gradle (GradleProject, discover, runGradle)
import System.FilePath qualified as FilePath
import Text.Pretty.Simple (pShow)
import Types (BuildTarget (..), DiscoveredProject (DiscoveredProject), FoundTargets (..))

gradleJarCallGraph ::
  ( Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  m CallGraphAnalysis
gradleJarCallGraph dir = do
  jars <- runReader (mempty :: AllFilters) $ getJarsByBuild dir
  parsedJars <- traverse callGraphFromJar jars
  pure $ JarAnalysis (catMaybes parsedJars)

getJarsByBuild ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [Path Abs File]
getJarsByBuild path = do
  discoveredProjects <- discover path
  jars <- traverse getJarsByBuildOfaProject discoveredProjects
  let candidateJars = concat jars

  logDebug . pretty $ ("Found jars: ") <> toText (pShow candidateJars)
  pure candidateJars

getJarsByBuildOfaProject ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  (DiscoveredProject GradleProject) ->
  m [Path Abs File]
getJarsByBuildOfaProject (DiscoveredProject _ path foundTargets _) = withSystemTempDir "fossa-gradle-reachability" $ \tmpDir -> do
  let jarScriptFilepath = fromAbsDir tmpDir FilePath.</> "jarpath.gradle"
  context "Writing gradle script" $ sendIO (BS.writeFile jarScriptFilepath initScript)

  let cmd :: Text.Text -> Command
      cmd = case foundTargets of
        FoundTargets targets -> gradleJarCmdTargets jarScriptFilepath (toSet targets)
        ProjectWithoutTargets -> gradleJarCmd jarScriptFilepath

  stdout <-
    context "running gradle script"
      . errCtx (FailedToRunGradleReachabilityAnalysisCtx path)
      . errHelp FailedToRunGradleReachabilityAnalysisHelp
      . errSupport (renderIt reportDefectWithDebugBundle)
      $ runGradle path cmd
  jarPathsFromScriptOutput stdout

jarPathsFromScriptOutput :: (Has Logger sig m, Has ReadFS sig m) => BL.ByteString -> m [Path Abs File]
jarPathsFromScriptOutput stdout = do
  let rawPaths :: [Text.Text] =
        map withoutJarFilePrefix
          . filter hasJarFilePrefix
          . Text.lines
          . decodeUtf8
          . BS.toStrict
          $ stdout

  logDebug . pretty $ ("Found candidate jars: ") <> toText (pShow rawPaths)
  case traverse (parseAbsFile . toString) rawPaths of
    Nothing -> pure []
    Just absJarPaths -> filterM isValidJar absJarPaths

jarFileSignifier :: Text.Text
jarFileSignifier = "JARFILE::"

hasJarFilePrefix :: Text.Text -> Bool
hasJarFilePrefix = Text.isPrefixOf jarFileSignifier

withoutJarFilePrefix :: Text.Text -> Text.Text
withoutJarFilePrefix t = fromMaybe t $ Text.stripPrefix jarFileSignifier t

initScript :: BS.ByteString
initScript = $(embedFile' "scripts/jarpaths.gradle")

-- | Run the init script on a set of subprojects. Note that this runs the
--  `:jarPaths` task on every subproject in one command. This is helpful for
--  performance reasons, because Gradle has a slow startup on each invocation.
gradleJarCmdTargets :: FilePath -> Set BuildTarget -> Text.Text -> Command
gradleJarCmdTargets initScriptFilepath targets baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["-I", toText initScriptFilepath] ++ map (\target -> unBuildTarget target <> ":jarPaths") (toList targets)
    , cmdAllowErr = Never
    }

gradleJarCmd :: FilePath -> Text.Text -> Command
gradleJarCmd initScriptFilepath baseCmd =
  Command
    { cmdName = baseCmd
    , cmdArgs = ["-I", toText initScriptFilepath, "jarPaths"]
    , cmdAllowErr = Never
    }

data FailedToRunGradleReachabilityAnalysis
  = FailedToRunGradleReachabilityAnalysisCtx (Path Abs Dir)
  | FailedToRunGradleReachabilityAnalysisHelp
  deriving (Eq, Ord, Show)
instance ToDiagnostic FailedToRunGradleReachabilityAnalysis where
  renderDiagnostic :: FailedToRunGradleReachabilityAnalysis -> Errata
  renderDiagnostic (FailedToRunGradleReachabilityAnalysisCtx path) =
    createErrataWithHeaderOnly $ "Failed to perform gradle analysis for " <> toText path
  renderDiagnostic FailedToRunGradleReachabilityAnalysisHelp = do
    let help =
          renderIt $
            vsep
              [ "Ensure your gradle project can be built successfully:"
              , ""
              , indent 2 "gradlew build"
              , indent 2 "gradlew.bat build"
              , indent 2 "gradle build"
              ]
    createErrataWithHeaderOnly help
