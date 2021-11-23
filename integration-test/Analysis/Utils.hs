{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Analysis.Utils (
  AnalysisIntegrationCase (..),
  FixtureEnvironment (..),
  FixtureArtifact (..),
  testSuiteOf,
  summarize,
  DependencyResultsTabulated (..),
  Assertion (..),
  AnalysisIntegrationCategory (..),
) where

import App.Fossa.Analyze.Types (AnalyzeExperimentalPreferences (AnalyzeExperimentalPreferences), AnalyzeProject (analyzeProject))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Finally (FinallyC, runFinally)
import Control.Carrier.Lift (Lift)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (interpret, sendSimple)
import Control.Effect.Diagnostics (FailureBundle)
import Control.Effect.Lift (sendIO)
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Binary qualified as CB
import Data.Foldable (for_)
import Data.Function ((&))
import Data.String.Conversion (toString)
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
import Graphing (directList, vertexList)
import Network.HTTP.Req (
  GET (GET),
  NoReqBody (NoReqBody),
  defaultHttpConfig,
  reqBr,
  runReq,
  useHttpsURI,
 )
import Network.HTTP.Req.Conduit (responseBodySource)
import Path (Abs, Dir, Path, Rel, reldir, toFilePath, (</>))
import Path.IO qualified as PIO
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe)
import Test.Hspec.Expectations.Pretty (shouldNotBe)
import Text.URI (mkURI)
import Types (
  DependencyResults (..),
  DiscoveredProject (
    projectBuildTargets,
    projectData,
    projectPath,
    projectType
  ),
  GraphBreadth (..),
 )

analysisIntegrationCaseFixtureDir :: Path Rel Dir
analysisIntegrationCaseFixtureDir = [reldir|integration-test/artifacts/|]

-- --------------------------------

data AnalysisIntegrationCase a = AnalysisIntegrationCase
  { testName :: Text
  , discover :: Path Abs Dir -> TestC IO [DiscoveredProject a]
  , environment :: FixtureEnvironment
  , buildCmd :: Maybe (Command, Path Rel Dir)
  , artifact :: FixtureArtifact
  , assertions :: [Assertion]
  }

-- | Fixture Environment to mimic when executing commands.
--
--  For nix packages:
--    @NixEnvSimpleConfig ["python3"]@
--
--  For nix raw expression:
--    @NixEnvRawExpression contentOfShell.nix file@
--
--  For local environment (ideal when ExecF is not used):
--    @LocalEnvironment@
--
--  TODO:
--    * Add DockerEnv Image MountDir
data FixtureEnvironment
  = NixEnvRawExpression Text
  | -- List of Nix Packages - e.g. python3, mix, etc.
    NixEnvSimpleConfig [Text]
  | LocalEnvironment
  deriving (Show, Eq, Ord)

data Assertion
  = Assertion
      -- Expected target type
      Text
      -- Expected path of project
      (Path Rel Dir)
      -- Expected dependency result tabulation
      DependencyResultsTabulated
  deriving (Show, Eq, Ord)

-- | Artifact to download and use for the test.
data FixtureArtifact = FixtureArtifact
  { -- Web url of the .tar.gz file
    tarGzFileUrl :: Text
  , -- Location to extract the archive artifact file
    extractAt :: Path Rel Dir
  }
  deriving (Show, Eq, Ord)

data DependencyResultsTabulated = DependencyResultsTabulated
  { nodes :: Int
  , directNodes :: Int
  , totalEdges :: Int
  , breadth :: GraphBreadth
  , countManifests :: Int
  }
  deriving (Show, Eq, Ord)

-- summarize :: DependencyResults -> DependencyResultsTabulated
-- summarize (DependencyResults g breadth manifests) =
--   DependencyResultsTabulated
--     (length . vertexList $ g)
--     (length . directList $ g)
--     (countEdges g)
--     breadth
--     (length manifests)

matchesProjectOf :: Text -> Path Abs Dir -> DiscoveredProject a -> Bool
matchesProjectOf expType expPath candidate =
  expType == (projectType candidate)
    && expPath == (projectPath candidate)

-- --------------------------------

-- hasTotalEdgesOf :: a -> Graphing a -> Expectation
-- hasTotalEdgesOf = a

-- hasTotalDirectDepsOf :: a -> Graphing a -> Expectation
-- hasTotalDirectDepsOf = a

-- hasTotalDeepDepsOf :: a -> Graphing a -> Expectation
-- hasTotalDeepDepsOf = a

-- --------------------------

-- testSuiteOf :: AnalyzeProject a => AnalysisIntegrationCase a -> Spec
testSuiteOf AnalysisIntegrationCase{..} = do
  downloadedArtifactFolder <- runIO (downloadArtifact artifact)

  -- Run build command to perform build within the environment
  runIO $ runCmd environment buildCmd
  discoveryResult <- runIO $ testRunnerWithLogger (discover downloadedArtifactFolder) environment)

--   describe (toString testName) $ do
--     discoveryResult <- runIO $ testRunnerWithLogger (discover downloadedArtifactFolder) environment
--     case discoveryResult of
--       Left f -> it "should discovery result" $ failed f
--       Right discoveredProjects -> do
        
--         for_ assertions $ \(Assertion expType expPath expSummary) -> do
--           absExpPath <- runIO $ PIO.makeAbsolute (analysisIntegrationCaseFixtureDir </> expPath)
--           let matchedProjects = Prelude.filter (matchesProjectOf expType absExpPath) discoveredProjects
--           it ("should find at-least one discovered project matching: " <> show (absExpPath, expType)) $
--             length matchedProjects `shouldNotBe` 0
          
--           -- For discovered project with same type, and project path
--           -- Assert dependency result summary
--           for_ matchedProjects $ \proj -> do
--             analysisResult <- runIO $ testRunnerWithLogger (ignoreDebug $ analyzeProject (projectBuildTargets proj) (projectData proj)) environment
--             it ("should match analyze result for: " <> show (projectPath proj, projectType proj)) $
--               case analysisResult of
--                 Left f -> failed f
--                 Right r -> expSummary `shouldBe` summarize r
  where
    runCmd :: FixtureEnvironment -> Maybe (Command, Path Rel Dir) -> IO ()
    runCmd env cmd =
      case cmd of
        Nothing -> pure ()
        Just (c, dir) -> do
          absDir <- PIO.makeAbsolute (analysisIntegrationCaseFixtureDir </> dir)
          res <- runExecIOWithinEnv env $ exec absDir c
          case res of
            Left err -> fail (show err)
            Right _ -> pure ()

    failed :: FailureBundle -> Expectation
    failed = expectationFailure . show

    downloadArtifact :: FixtureArtifact -> IO (Path Abs Dir)
    downloadArtifact target = sendIO $ do
      -- Ensure parent directory for fixture exists
      PIO.ensureDir analysisIntegrationCaseFixtureDir
      let artifactUrl = tarGzFileUrl target
      let artifactExtraction = extractAt target
      let archiveExtractionDir = analysisIntegrationCaseFixtureDir </> artifactExtraction

      -- Ensure test artifacts are always fresh!
      PIO.ensureDir archiveExtractionDir
      PIO.removeDirRecur archiveExtractionDir

      resolvedUrl <- useHttpsURI <$> mkURI artifactUrl
      case resolvedUrl of
        Nothing -> fail ("could not be resolved, artifact's download url: " <> show artifactUrl)
        Just (url, _) -> do
          (artifactFile, _) <- PIO.openTempFile analysisIntegrationCaseFixtureDir "artifact-integration"
          _ <- runReq defaultHttpConfig $
            reqBr GET (url) NoReqBody mempty $
              \r -> runConduitRes $ responseBodySource r .| CB.sinkFile (toFilePath artifactFile)

          -- Extract and remove downloaded archive file
          PIO.createDir archiveExtractionDir
          archiveExtractFolder <- PIO.makeAbsolute archiveExtractionDir
          extractTarGz archiveExtractFolder artifactFile
          PIO.removeFile artifactFile

          pure archiveExtractFolder

-- --------------------------------

type TestC m a = ExecIOC (ReadFSIOC (DiagnosticsC (LoggerC ((ReaderC (AnalyzeExperimentalPreferences)) (FinallyC m))))) a

testRunnerWithLogger :: (Has (Lift IO) sig m) => TestC m a -> FixtureEnvironment -> m (Either FailureBundle a)
testRunnerWithLogger f env =
  f
    & (runExecIOWithinEnv env)
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
decorateCmdWith (LocalEnvironment) cmd = cmd
