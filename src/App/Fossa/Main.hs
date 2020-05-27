
module App.Fossa.Main
  ( appMain
  ) where

import Prologue

import App.Fossa.Analyze (analyzeMain, ScanDestination(..))
import App.Fossa.FossaAPIV1 (ProjectMetadata(..))
import App.Fossa.Test (testMain)
import Effect.Logger
import Options.Applicative
import Path.IO (resolveDir', doesDirExist, setCurrentDir)
import System.Environment (lookupEnv)
import System.Exit (die)
import qualified Data.Text as T
import OptionExtensions
import Network.HTTP.Req (https)

appMain :: IO ()
appMain = do
  CmdOptions{..} <- customExecParser (prefs (showHelpOnError <> subparserInline)) (info (opts <**> helper) (fullDesc <> header "fossa-cli - Flexible, performant dependency analysis"))
  let logSeverity = bool SevInfo SevDebug optDebug

  maybeApiKey <- checkAPIKey optAPIKey

  case optCommand of
    AnalyzeCommand AnalyzeOptions{..} -> do
      changeDir analyzeBaseDir
      if analyzeOutput
        then analyzeMain logSeverity OutputStdout optProjectName optProjectRevision
        else case maybeApiKey of
          Nothing -> die "A FOSSA API key is required to run this command"
          Just key -> analyzeMain logSeverity (UploadScan optBaseUrl key analyzeMetadata) optProjectName optProjectRevision

    TestCommand TestOptions{..} -> do
      changeDir testBaseDir
      case maybeApiKey of
        Nothing -> die "A FOSSA API key is required to run this command"
        Just key -> testMain optBaseUrl key logSeverity testTimeout optProjectName optProjectRevision

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
    <*> urlOption (long "endpoint" <> metavar "URL" <> help "The FOSSA API server base URL" <> value urlOpts)
    <*> optional (strOption (long "project" <> help "this repository's URL or VCS endpoint (default: VCS remote 'origin')"))
    <*> optional (strOption (long "revision" <> help "this repository's current revision hash (default: VCS hash HEAD)"))
    <*> optional (strOption (long "fossa-api-key" <> help "the FOSSA API server authenticaion key (default: FOSSA_API_KEY from env)"))
    <*> comm
    <**> helper
    where
      baseUrl = https "app.fossa.com"
      urlOpts = UrlOption baseUrl mempty

comm :: Parser Command
comm = hsubparser
  ( command "analyze"
    ( info
        (AnalyzeCommand <$> analyzeOpts)
        (progDesc "Scan for projects and their dependencies")
    )
  <> command "test"
    ( info
        (TestCommand <$> testOpts)
        (progDesc "Check for issues from FOSSA and exit non-zero when issues are found")
    )
  )

analyzeOpts :: Parser AnalyzeOptions
analyzeOpts =
  AnalyzeOptions
    <$> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")
    <*> metadataOpts
    <*> baseDirArg

metadataOpts :: Parser ProjectMetadata
metadataOpts =
  ProjectMetadata
    <$> optional (strOption (long "title" <> help "the title of the FOSSA project. (default: the project name)"))
    <*> optional (strOption (long "branch" <> help "this repository's current branch (default: current VCS branch)"))
    <*> optional (strOption (long "project-url" <> help "this repository's home page"))
    <*> optional (strOption (long "jira-project-key" <> help "this repository's JIRA project key"))
    <*> optional (strOption (long "link" <> help "a link to attach to the current build"))
    <*> optional (strOption (long "team" <> help "this repository's team inside your organization"))
    <*> optional (strOption (long "policy" <> help "the policy to assign to this project in FOSSA"))

testOpts :: Parser TestOptions
testOpts =
  TestOptions
    <$> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> baseDirArg

data CmdOptions = CmdOptions
  { optDebug   :: Bool
  , optBaseUrl :: UrlOption
  , optProjectName :: Maybe Text
  , optProjectRevision :: Maybe Text
  , optAPIKey :: Maybe Text
  , optCommand :: Command
  }

data Command
  = AnalyzeCommand AnalyzeOptions
  | TestCommand TestOptions

data AnalyzeOptions = AnalyzeOptions
  { analyzeOutput :: Bool
  , analyzeMetadata :: ProjectMetadata
  , analyzeBaseDir :: FilePath
  }

data TestOptions = TestOptions
  { testTimeout :: Int
  , testBaseDir :: FilePath
  }
