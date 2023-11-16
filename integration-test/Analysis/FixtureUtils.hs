{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Analysis.FixtureUtils (
  AnalysisTestFixture (..),
  FixtureEnvironment (..),
  FixtureArtifact (..),
  TestC,
  performDiscoveryAndAnalyses,
  getArtifact,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject))
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig (ExperimentalAnalyzeConfig), GoDynamicTactic (GoModulesBasedTactic))
import App.Types (OverrideDynamicAnalysisBinary)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Finally (FinallyC, runFinally)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (interpret, sendSimple)
import Control.Carrier.Stack (StackC, runStack)
import Control.Carrier.Telemetry (
  IgnoreTelemetryC,
  withoutTelemetry,
 )
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Binary qualified as CB
import Data.Function ((&))
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Diag.Result (EmittedWarn, Result (Failure, Success), renderFailure)
import Discovery.Archive (selectUnarchiver)
import Discovery.Filters (AllFilters, MavenScopeFilters)
import Effect.Exec (
  Command (..),
  ExecF (Exec),
  ExecIOC,
  Has,
  exec,
  runExecIO,
 )
import Effect.Logger (LoggerC, Severity (SevWarn), withDefaultLogger)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Network.HTTP.Req (
  GET (GET),
  NoReqBody (NoReqBody),
  Url,
  defaultHttpConfig,
  renderUrl,
  reqBr,
  runReq,
  useHttpsURI,
 )
import Network.HTTP.Req.Conduit (responseBodySource)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  reldir,
  toFilePath,
  (</>),
 )
import Path.IO qualified as PIO
import System.Directory.Internal.Prelude (Handle, hClose)
import Text.URI (mkURI)
import Type.Operator (type ($))
import Types (
  DependencyResults,
  DiscoveredProject (projectBuildTargets, projectData),
 )

analysisIntegrationCaseFixtureDir :: Path Rel Dir
analysisIntegrationCaseFixtureDir = [reldir|integration-test/artifacts/|]

-- | Represents analysis test fixture.
data AnalysisTestFixture a = AnalysisTestFixture
  { testName :: Text
  , discover :: Path Abs Dir -> TestC IO [DiscoveredProject a]
  , environment :: FixtureEnvironment
  , buildCmd :: Maybe Command
  , artifact :: FixtureArtifact
  }

-- | Fixture Environment to mimic when executing commands.
data FixtureEnvironment
  = LocalEnvironment
  | NixEnv [Text]
  deriving (Show, Eq, Ord)

-- | Artifact to download and use for the test.
data FixtureArtifact = FixtureArtifact
  { tarGzFileUrl :: Text
  , extractAt :: Path Rel Dir
  , scopedDir :: Path Rel Dir
  }
  deriving (Show, Eq, Ord)

type TestC m =
  ExecIOC
    $ ReadFSIOC
    $ DiagnosticsC
    $ LoggerC
    $ ReaderC OverrideDynamicAnalysisBinary
    $ ReaderC AllFilters
    $ ReaderC MavenScopeFilters
    $ ReaderC ExperimentalAnalyzeConfig
    $ FinallyC
    $ StackC
    $ IgnoreTelemetryC m

testRunnerWithLogger :: TestC IO a -> FixtureEnvironment -> IO (Result a)
testRunnerWithLogger f env =
  f
    & runExecIOWithinEnv env
    & runReadFSIO
    & runDiagnostics
    & withDefaultLogger SevWarn
    & runReader (mempty :: OverrideDynamicAnalysisBinary)
    & runReader (mempty :: AllFilters)
    & runReader (mempty :: MavenScopeFilters)
    & runReader (ExperimentalAnalyzeConfig Nothing GoModulesBasedTactic)
    & runFinally
    & runStack
    & withoutTelemetry

runExecIOWithinEnv :: (Has (Lift IO) sig m) => FixtureEnvironment -> ExecIOC m a -> m a
runExecIOWithinEnv conf = interpret $ \case
  Exec dir cmd stdin -> sendIO $ runExecIO $ sendSimple (Exec dir (decorateCmdWith conf cmd) stdin)

decorateCmdWith :: FixtureEnvironment -> Command -> Command
decorateCmdWith LocalEnvironment cmd = cmd
decorateCmdWith (NixEnv pkgs) cmd =
  Command
    { cmdName = "nix-shell"
    , cmdArgs = ["-p"] <> pkgs <> ["--run"] <> [cmdName cmd <> " " <> Text.intercalate " " (cmdArgs cmd)]
    , cmdAllowErr = cmdAllowErr cmd
    }

-- --------------------------------
-- Analysis fixture test runner

performDiscoveryAndAnalyses :: (Has (Lift IO) sig m, AnalyzeProject a, MonadFail m) => Path Abs Dir -> AnalysisTestFixture a -> m [(DiscoveredProject a, DependencyResults)]
performDiscoveryAndAnalyses targetDir AnalysisTestFixture{..} = do
  -- Perform any project builds
  _ <- sendIO $ runCmd environment buildCmd

  -- Perform discovery
  discoveryResult <- sendIO $ testRunnerWithLogger (discover targetDir) environment
  withResult discoveryResult $ \_ dps ->
    for dps $ \dp -> do
      analysisResult <- sendIO $ testRunnerWithLogger (ignoreDebug $ analyzeProject (projectBuildTargets dp) (projectData dp)) environment
      withResult analysisResult $ \_ dr -> pure (dp, dr)
  where
    runCmd :: FixtureEnvironment -> Maybe (Command) -> IO ()
    runCmd env cmd =
      case cmd of
        Nothing -> pure ()
        Just c -> do
          res <- runExecIOWithinEnv env $ exec (targetDir </> scopedDir artifact) c
          case res of
            Left err -> fail (show err)
            Right _ -> pure ()

-- TODO: don't use MonadFail here
withResult :: MonadFail m => Result a -> ([EmittedWarn] -> a -> m b) -> m b
withResult (Failure ws eg) _ = fail (show (renderFailure ws eg "An issue occurred"))
withResult (Success ws a) f = f ws a

-- --------------------------------
-- IO helpers for test runners

getArtifact :: Has (Lift IO) sig m => FixtureArtifact -> m (Path Abs Dir)
getArtifact target = sendIO $ do
  -- Ensure parent directory for fixture exists
  PIO.ensureDir analysisIntegrationCaseFixtureDir
  let archiveExtractionDir = analysisIntegrationCaseFixtureDir </> extractAt target

  PIO.ensureDir archiveExtractionDir
  resolvedUrl <- useHttpsURI <$> mkURI artifactUrl

  case resolvedUrl of
    Nothing -> fail ("could not be resolved, artifact's download url: " <> show artifactUrl)
    Just (url, _) -> do
      res <- PIO.withTempFile (analysisIntegrationCaseFixtureDir) "artifact" (downloadAndExtractArtifact url archiveExtractionDir)
      either fail pure res
  where
    artifactUrl = tarGzFileUrl target

    downloadAndExtractArtifact :: Url a -> Path Rel Dir -> Path Abs File -> Handle -> IO (Either String (Path Abs Dir))
    downloadAndExtractArtifact url extractionTarget tempFile tempFileHandle = do
      _ <- runReq defaultHttpConfig $
        reqBr GET url NoReqBody mempty $
          \r -> runConduitRes $ responseBodySource r .| CB.sinkFileCautious (toFilePath tempFile)

      sendIO $ hClose tempFileHandle

      sendIO $ PIO.ensureDir extractionTarget
      archiveExtractFolder <- sendIO $ PIO.makeAbsolute extractionTarget

      let urlStr = toString . renderUrl $ url
      case selectUnarchiver urlStr of
        Nothing -> pure . Left $ "Failed to extract file from " <> urlStr
        Just extractor -> do
          sendIO $ extractor archiveExtractFolder tempFile
          pure . Right $ archiveExtractFolder
