{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Analysis.FixtureUtils (
  AnalysisTestFixture (..),
  FixtureEnvironment (..),
  FixtureArtifact (..),
  performDiscoveryAndAnalyses,
  getArtifact,
) where

import App.Fossa.Analyze.Types (AnalyzeExperimentalPreferences (AnalyzeExperimentalPreferences), AnalyzeProject (analyzeProject))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Finally (FinallyC, runFinally)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (interpret, sendSimple)
import Control.Effect.Diagnostics (FailureBundle)
import Control.Monad (forM)
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Binary qualified as CB
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Archive (extractTarGz)
import Effect.Exec (
  Command (..),
  ExecF (Exec),
  ExecIOC,
  Has,
  exec,
  runExecIO,
 )
import Effect.Logger (LoggerC, Severity (SevDebug), withDefaultLogger)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Network.HTTP.Req (
  GET (GET),
  NoReqBody (NoReqBody),
  defaultHttpConfig,
  reqBr,
  runReq,
  useHttpsURI,
  Url,
 )
import Network.HTTP.Req.Conduit (responseBodySource)
import Path
import Path.IO qualified as PIO
import Text.URI (mkURI)
import Types
import System.Directory.Internal.Prelude (hClose, Handle)

analysisIntegrationCaseFixtureDir :: Path Rel Dir
analysisIntegrationCaseFixtureDir = [reldir|integration-test/artifacts/|]

-- | Represents analysis test fixture.
data AnalysisTestFixture a = AnalysisTestFixture
  { testName :: Text
  , discover :: Path Abs Dir -> TestC IO [DiscoveredProject a]
  , environment :: FixtureEnvironment
  , buildCmd :: Maybe (Command, Path Rel Dir)
  , artifact :: FixtureArtifact
  }


-- | Fixture Environment to mimic when executing commands.
data FixtureEnvironment
  = NixEnvRawExpression Text
  | NixEnvSimpleConfig [Text]
  | LocalEnvironment
  deriving (Show, Eq, Ord)

-- | Artifact to download and use for the test.
data FixtureArtifact = FixtureArtifact
  { tarGzFileUrl :: Text
  , extractAt :: Path Rel Dir
  , scopedDir :: Maybe (Path Rel Dir)
  }
  deriving (Show, Eq, Ord)

type TestC m a = ExecIOC (ReadFSIOC (DiagnosticsC (LoggerC ((ReaderC AnalyzeExperimentalPreferences) (FinallyC m))))) a

testRunnerWithLogger :: (Has (Lift IO) sig m) => TestC m a -> FixtureEnvironment -> m (Either FailureBundle a)
testRunnerWithLogger f env =
  f
    & runExecIOWithinEnv env
    & runReadFSIO
    & runDiagnostics
    & withDefaultLogger SevDebug
    & runReader (AnalyzeExperimentalPreferences Nothing)
    & runFinally

runExecIOWithinEnv :: (Has (Lift IO) sig m) => FixtureEnvironment -> ExecIOC m a -> m a
runExecIOWithinEnv conf = interpret $ \case
  Exec dir cmd -> sendIO $ runExecIO $ sendSimple (Exec dir $ decorateCmdWith conf cmd)

decorateCmdWith :: FixtureEnvironment -> Command -> Command
decorateCmdWith (NixEnvSimpleConfig pkgs) cmd =
  Command
    { cmdName = "nix-shell"
    , cmdArgs = ["-p"] <> pkgs <> ["--run"] <> [cmdName cmd <> " " <> Text.intercalate " " (cmdArgs cmd)]
    , cmdAllowErr = cmdAllowErr cmd
    }
decorateCmdWith (NixEnvRawExpression nixExpression) cmd =
  Command
    { cmdName = "nix-shell"
    , cmdArgs = ["-I"] <> [nixExpression] <> ["--run"] <> [cmdName cmd <> " " <> Text.intercalate " " (cmdArgs cmd)]
    , cmdAllowErr = cmdAllowErr cmd
    }
decorateCmdWith LocalEnvironment cmd = cmd

-- --------------------------------
-- Analysis fixture test runner

performDiscoveryAndAnalyses :: (Has (Lift IO) sig m, AnalyzeProject a, MonadFail m) => AnalysisTestFixture a -> m [(DiscoveredProject a, DependencyResults)]
performDiscoveryAndAnalyses AnalysisTestFixture{..} = do
  sendIO $ runCmd environment buildCmd
  absoluteArtifactDir <- absoluteDirOf (extractAt artifact)
  discoveryResult <- sendIO $ testRunnerWithLogger (discover absoluteArtifactDir) environment
  case discoveryResult of
    Left fb -> fail (show fb)
    Right dps ->
      forM dps $ \dp -> do
        analysisResult <- sendIO $ testRunnerWithLogger (ignoreDebug $ analyzeProject (projectBuildTargets dp) (projectData dp)) environment
        case analysisResult of
          Left fb -> fail (show fb)
          Right dr -> pure (dp, dr)
  where
    runCmd :: FixtureEnvironment -> Maybe (Command, Path Rel Dir) -> IO ()
    runCmd env cmd =
      case cmd of
        Nothing -> pure ()
        Just (c, dir) -> do
          absDir <- absoluteDirOf dir
          res <- runExecIOWithinEnv env $ exec absDir c
          case res of
            Left err -> fail (show err)
            Right _ -> pure ()

-- --------------------------------
-- IO helpers for test runners

absoluteDirOf :: Has (Lift IO) sig m => Path Rel Dir -> m (Path Abs Dir)
absoluteDirOf relDir = sendIO $ PIO.makeAbsolute (analysisIntegrationCaseFixtureDir </> relDir)

getArtifact :: Has (Lift IO) sig m => FixtureArtifact -> m (Path Abs Dir)
getArtifact target = sendIO $ do
  -- Ensure parent directory for fixture exists
  PIO.ensureDir analysisIntegrationCaseFixtureDir
  let artifactUrl = tarGzFileUrl target
  let archiveExtractionDir = analysisIntegrationCaseFixtureDir </> extractAt target

  PIO.ensureDir archiveExtractionDir
  resolvedUrl <- useHttpsURI <$> mkURI artifactUrl

  case resolvedUrl of
    Nothing -> fail ("could not be resolved, artifact's download url: " <> show artifactUrl)
    Just (url, _) -> do
      PIO.withTempFile (analysisIntegrationCaseFixtureDir) "artifact" (downloadAndExtractArtifact url archiveExtractionDir)
  
  where
    downloadAndExtractArtifact :: Url a -> Path Rel Dir -> Path Abs File -> Handle -> IO (Path Abs Dir)
    downloadAndExtractArtifact url extractAt tempFile tempFileHandle = do
      _ <- runReq defaultHttpConfig $
        reqBr GET url NoReqBody mempty $
        \r -> runConduitRes $ responseBodySource r .| CB.sinkFileCautious (toFilePath tempFile)

      sendIO $ hClose tempFileHandle

      sendIO $ PIO.ensureDir extractAt
      archiveExtractFolder <- sendIO $ PIO.makeAbsolute extractAt
      sendIO $ extractTarGz archiveExtractFolder tempFile

      PIO.removeFile tempFile
      pure archiveExtractFolder
