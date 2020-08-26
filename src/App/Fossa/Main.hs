{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.Main
  ( appMain,
  )
where

import App.Fossa.Analyze (ScanDestination (..), analyzeMain)
import App.Fossa.FossaAPIV1 (ProjectMetadata (..))
import App.Fossa.Report (ReportType (..), reportMain)
import App.Fossa.Test (TestOutputType (..), testMain)
import App.OptionExtensions
import App.Types
import App.Util (validateDir)
import Control.Monad (unless)
import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as T
import Effect.Logger
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.URI (URI)
import Text.URI.QQ (uri)

appMain :: IO ()
appMain = do
  CmdOptions {..} <- customExecParser (prefs (showHelpOnError <> subparserInline)) (info (opts <**> helper) (fullDesc <> header "fossa-cli - Flexible, performant dependency analysis"))
  let logSeverity = bool SevInfo SevDebug optDebug
  --
  maybeApiKey <- checkAPIKey optAPIKey
  let override =
        OverrideProject
          { overrideName = optProjectName,
            overrideRevision = optProjectRevision,
            overrideBranch = Nothing
          }
  --
  case optCommand of
    AnalyzeCommand AnalyzeOptions {..} -> do
      baseDir <- validateDir analyzeBaseDir
      let analyzeOverride = override {overrideBranch = analyzeBranch}
      if analyzeOutput
        then analyzeMain baseDir logSeverity OutputStdout analyzeOverride analyzeUnpackArchives
        else do
          key <- requireKey maybeApiKey
          analyzeMain baseDir logSeverity (UploadScan optBaseUrl key analyzeMetadata) analyzeOverride analyzeUnpackArchives
    --
    TestCommand TestOptions {..} -> do
      baseDir <- validateDir testBaseDir
      key <- requireKey maybeApiKey
      testMain optBaseUrl baseDir key logSeverity testTimeout testOutputType override
    --
    InitCommand ->
      withLogger logSeverity $ logWarn "This command has been deprecated and is no longer needed.  It has no effect and may be safely removed."
    --
    ReportCommand ReportOptions {..} -> do
      unless reportJsonOutput $ die "report command currently only supports JSON output.  Please try `fossa report --json REPORT_NAME`"
      baseDir <- validateDir reportBaseDir
      key <- requireKey maybeApiKey
      reportMain optBaseUrl baseDir key logSeverity reportTimeout reportType override

requireKey :: Maybe ApiKey -> IO ApiKey
requireKey (Just key) = pure key
requireKey Nothing = die "A FOSSA API key is required to run this command"

infixl 4 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-- | Try to fetch FOSSA_API_KEY from env if not supplied from cmdline
checkAPIKey :: Maybe Text -> IO (Maybe ApiKey)
checkAPIKey key = case key of
  Just key' -> pure . Just $ ApiKey key'
  Nothing -> ApiKey . T.pack <$$> lookupEnv "FOSSA_API_KEY"

baseDirArg :: Parser String
baseDirArg = argument str (metavar "DIR" <> help "Set the base directory for scanning (default: current directory)" <> value ".")

opts :: Parser CmdOptions
opts =
  CmdOptions
    <$> switch (long "debug" <> help "Enable debug logging")
    <*> uriOption (long "endpoint" <> metavar "URL" <> help "The FOSSA API server base URL" <> value [uri|https://app.fossa.com|])
    <*> optional (strOption (long "project" <> help "this repository's URL or VCS endpoint (default: VCS remote 'origin')"))
    <*> optional (strOption (long "revision" <> help "this repository's current revision hash (default: VCS hash HEAD)"))
    <*> optional (strOption (long "fossa-api-key" <> help "the FOSSA API server authenticaion key (default: FOSSA_API_KEY from env)"))
    <*> comm
    <**> helper

comm :: Parser Command
comm =
  hsubparser
    ( command
        "analyze"
        ( info
            (AnalyzeCommand <$> analyzeOpts)
            (progDesc "Scan for projects and their dependencies")
        )
        <> command
          "test"
          ( info
              (TestCommand <$> testOpts)
              (progDesc "Check for issues from FOSSA and exit non-zero when issues are found")
          )
        <> command
          "report"
          ( info
              (ReportCommand <$> reportOpts)
              (progDesc "Access various reports from FOSSA and print to stdout")
          )
        <> command
          "init"
          ( info
              (pure InitCommand)
              (progDesc "Deprecated, has no effect.")
          )
    )

analyzeOpts :: Parser AnalyzeOptions
analyzeOpts =
  AnalyzeOptions
    <$> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")
    <*> switch (long "unpack-archives" <> help "Recursively unpack and analyze discovered archives")
    <*> optional (strOption (long "branch" <> help "this repository's current branch (default: current VCS branch)"))
    <*> metadataOpts
    <*> baseDirArg

metadataOpts :: Parser ProjectMetadata
metadataOpts =
  ProjectMetadata
    <$> optional (strOption (long "title" <> help "the title of the FOSSA project. (default: the project name)"))
    <*> optional (strOption (long "project-url" <> help "this repository's home page"))
    <*> optional (strOption (long "jira-project-key" <> help "this repository's JIRA project key"))
    <*> optional (strOption (long "link" <> help "a link to attach to the current build"))
    <*> optional (strOption (long "team" <> help "this repository's team inside your organization"))
    <*> optional (strOption (long "policy" <> help "the policy to assign to this project in FOSSA"))

reportOpts :: Parser ReportOptions
reportOpts =
  ReportOptions
    <$> switch (long "json" <> help "Output the report in JSON format (Currently required).")
    <*> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> reportCmd
    <*> baseDirArg

reportCmd :: Parser ReportType
reportCmd =
  hsubparser $
    command "attribution" (info (pure AttributionReport) $ progDesc "Generate attribution report" )

testOpts :: Parser TestOptions
testOpts =
  TestOptions
    <$> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json")
    <*> baseDirArg

data CmdOptions = CmdOptions
  { optDebug :: Bool,
    optBaseUrl :: URI,
    optProjectName :: Maybe Text,
    optProjectRevision :: Maybe Text,
    optAPIKey :: Maybe Text,
    optCommand :: Command
  }

data Command
  = AnalyzeCommand AnalyzeOptions
  | TestCommand TestOptions
  | ReportCommand ReportOptions
  | InitCommand

data ReportOptions = ReportOptions
  { reportJsonOutput :: Bool,
    reportTimeout :: Int,
    reportType :: ReportType,
    reportBaseDir :: FilePath
  }

data AnalyzeOptions = AnalyzeOptions
  { analyzeOutput :: Bool,
    analyzeUnpackArchives :: Bool,
    analyzeBranch :: Maybe Text,
    analyzeMetadata :: ProjectMetadata,
    analyzeBaseDir :: FilePath
  }

data TestOptions = TestOptions
  { testTimeout :: Int,
    testOutputType :: TestOutputType,
    testBaseDir :: FilePath
  }
