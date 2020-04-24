
module App.Fossa.Main
  ( appMain
  ) where

import Prologue

import App.Fossa.Analyze (analyzeMain, ScanDestination(..))
import App.Fossa.FossaAPIV1 (ProjectMetadata(..))
import App.Fossa.Test (testMain)
import Effect.Logger
import Network.HTTP.Req (useURI, https, Url, Scheme(..))
import Options.Applicative
import Path.IO (resolveDir', doesDirExist, setCurrentDir)
import System.Environment (lookupEnv)
import System.Exit (die)
import qualified Data.Text as T
import Text.URI (mkURI)

appMain :: IO ()
appMain = do
  CmdOptions{..} <- customExecParser (prefs showHelpOnError) (info (opts <**> helper) (fullDesc <> header "fossa-cli - Flexible, performant dependency analysis"))
  let logSeverity = bool SevInfo SevDebug optDebug

  maybeApiKey <- fmap T.pack <$> lookupEnv "FOSSA_API_KEY"

  basedir <- validateDir optBasedir
 
  setCurrentDir basedir

  case optCommand of
    AnalyzeCommand AnalyzeOptions{..} ->
      if analyzeOutput
        then analyzeMain logSeverity OutputStdout optProjectName optProjectRevision
        else case maybeApiKey of
          Nothing -> die "A FOSSA API key is required to run this command"
          Just key -> analyzeMain logSeverity (UploadScan optBaseUrl key analyzeMetadata) optProjectName optProjectRevision

    TestCommand TestOptions{..} ->
      case maybeApiKey of
        Nothing -> die "A FOSSA API key is required to run this command"
        Just key -> testMain optBaseUrl key logSeverity testTimeout optProjectName optProjectRevision

-- | Validate that a filepath points to a directory and the directory exists
validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute

opts :: Parser CmdOptions
opts =
  CmdOptions
    <$> switch (long "debug" <> help "Enable debug logging")
    <*> strOption (long "basedir" <> short 'd' <> metavar "DIR" <> help "Set the base directory for scanning (default: current directory)" <> value ".")
    <*> urlOption (long "endpoint" <> metavar "URL" <> help "The FOSSA API server base URL" <> value (https "app.fossa.com"))
    <*> optional (strOption (long "project" <> help "this repository's URL or VCS endpoint (default: VCS remote 'origin')"))
    <*> optional (strOption (long "revision" <> help "this repository's current revision hash (default: VCS hash HEAD)"))
    <*> comm
    <**> helper

urlOption :: Mod OptionFields (Url scheme) -> Parser (Url scheme)
urlOption = option parseUrl
  where
    parseUrl :: ReadM (Url scheme)
    parseUrl = maybeReader (\s -> mkURI (T.pack s) >>= useURI >>= \res ->
                               case res of
                                 Left (url,_) -> pure $ coerce url
                                 Right (url,_) -> pure $ coerce url)

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

data CmdOptions = CmdOptions
  { optDebug   :: Bool
  , optBasedir :: FilePath
  , optBaseUrl :: Url 'Https
  , optProjectName :: Maybe Text
  , optProjectRevision :: Maybe Text
  , optCommand :: Command
  }

data Command
  = AnalyzeCommand AnalyzeOptions
  | TestCommand TestOptions

data AnalyzeOptions = AnalyzeOptions
  { analyzeOutput :: Bool
  , analyzeMetadata :: ProjectMetadata
  }

data TestOptions = TestOptions
  { testTimeout :: Int
  }
