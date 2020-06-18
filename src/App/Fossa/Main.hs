{-# language QuasiQuotes #-}

module App.Fossa.Main
  ( appMain,
  )
where

import App.Fossa.Analyze (ScanDestination (..), analyzeMain)
import App.Fossa.FossaAPIV1 (ProjectMetadata (..))
import App.Fossa.Report (ReportType (..), reportMain)
import App.Fossa.Test (TestOutputType (..), testMain)
import qualified Data.Text as T
import OptionExtensions
import Effect.Logger
import Options.Applicative
import Path.IO (doesDirExist, resolveDir', setCurrentDir)
import Prologue
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.URI (URI)
import Text.URI.QQ (uri)

appMain :: IO ()
appMain = do
  CmdOptions {..} <- customExecParser (prefs (showHelpOnError <> subparserInline)) (info (opts <**> helper) (fullDesc <> header "fossa-cli - Flexible, performant dependency analysis"))
  let logSeverity = bool SevInfo SevDebug optDebug

  maybeApiKey <- checkAPIKey optAPIKey

  case optCommand of
    AnalyzeCommand AnalyzeOptions {..} -> do
      changeDir analyzeBaseDir
      if analyzeOutput
        then analyzeMain logSeverity OutputStdout optProjectName optProjectRevision analyzeBranch
        else do
          key <- requireKey maybeApiKey
          analyzeMain logSeverity (UploadScan optBaseUrl key analyzeMetadata) optProjectName optProjectRevision analyzeBranch
    TestCommand TestOptions {..} -> do
      changeDir testBaseDir
      key <- requireKey maybeApiKey
      testMain optBaseUrl key logSeverity testTimeout testOutputType optProjectName optProjectRevision
    InitCommand ->
      withLogger logSeverity $ logWarn "This command has been deprecated and is no longer needed.  It has no effect and may be safely removed."
    ReportCommand ReportOptions {..} -> do
      unless reportJsonOutput $ die "report command currently only supports JSON output.  Please try `fossa report --json REPORT_NAME`"
      changeDir reportBaseDir
      key <- requireKey maybeApiKey
      reportMain optBaseUrl key logSeverity reportTimeout reportType optProjectName optProjectRevision
      

requireKey :: Maybe Text -> IO Text
requireKey (Just key) = pure key
requireKey Nothing = die "A FOSSA API key is required to run this command"

changeDir :: FilePath -> IO ()
changeDir path = validateDir path >>= setCurrentDir

-- | Try to fetch FOSSA_API_KEY from env if not supplied from cmdline
checkAPIKey :: Maybe Text -> IO (Maybe Text)
checkAPIKey Nothing = fmap T.pack <$> lookupEnv "FOSSA_API_KEY"
checkAPIKey key = return key

-- | Validate that a filepath points to a directory and the directory exists
validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute

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
    analyzeBranch :: Maybe Text,
    analyzeMetadata :: ProjectMetadata,
    analyzeBaseDir :: FilePath
  }

data TestOptions = TestOptions
  { testTimeout :: Int,
    testOutputType :: TestOutputType,
    testBaseDir :: FilePath
  }
