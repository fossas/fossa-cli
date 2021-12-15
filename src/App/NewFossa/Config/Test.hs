{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.NewFossa.Config.Test (
  TestCliOpts,
  TestConfig (..),
  OutputFormat (..),
  TimeoutSeconds (..),
  mkSubCommand,
) where

import App.NewFossa.Config.Common (
  GlobalOpts (..),
  baseDirArg,
  collectBaseDir,
  collectRevisionData,
  globalOpts,
  validateApiKey,
 )
import App.NewFossa.ConfigFile (ConfigFile, resolveConfigFile)
import App.NewFossa.EnvironmentVars (EnvVars)
import App.NewFossa.Subcommand (EffStack, GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (BaseDir, OverrideProject (OverrideProject), ProjectRevision)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  Validator,
  runValidation,
  validationBoundary,
 )
import Control.Effect.Lift (Lift, sendIO)
import Effect.Exec (Exec)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts (ApiOpts))
import Options.Applicative (
  InfoMod,
  Parser,
  auto,
  flag,
  help,
  long,
  option,
  progDesc,
  value,
 )
import Path.IO (getCurrentDir)

data OutputFormat
  = TestOutputPretty
  | TestOutputJson
  deriving (Eq, Ord, Show)

newtype TimeoutSeconds = TimeoutSeconds
  { unTimeoutSeconds :: Int
  }
  deriving (Eq, Ord, Show)

data TestCliOpts = TestCliOpts
  { globals :: GlobalOpts
  , testTimeout :: Int
  , testOutputType :: OutputFormat
  , testBaseDir :: FilePath
  }
  deriving (Eq, Ord, Show)

instance GetSeverity TestCliOpts where
  getSeverity TestCliOpts{globals = GlobalOpts{optDebug}} = if optDebug then SevDebug else SevInfo

data TestConfig = TestConfig
  { baseDir :: BaseDir
  , apiOpts :: ApiOpts
  , timeoutSeconds :: TimeoutSeconds
  , outputFormat :: OutputFormat
  , projectRevision :: ProjectRevision
  }

testInfo :: InfoMod a
testInfo = progDesc "Check for issues from FOSSA and exit non-zero when issues are found"

mkSubCommand :: (TestConfig -> EffStack ()) -> SubCommand TestCliOpts TestConfig
mkSubCommand = SubCommand "test" testInfo parser loadConfig mergeOpts

parser :: Parser TestCliOpts
parser =
  TestCliOpts
    <$> globalOpts
    <*> option
      auto
      ( mconcat
          [ long "timeout"
          , help "Duration to wait for build completion (in seconds)"
          , value oneHourInSeconds
          ]
      )
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json")
    <*> baseDirArg

oneHourInSeconds :: Int
oneHourInSeconds = 60 * 60

loadConfig ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  TestCliOpts ->
  m (Maybe ConfigFile)
loadConfig TestCliOpts{globals = GlobalOpts{optConfig}} = do
  -- FIXME: We eventually want to use the basedir to inform the config file root
  configRelBase <- sendIO getCurrentDir
  resolveConfigFile configRelBase optConfig

mergeOpts ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  TestCliOpts ->
  m TestConfig
mergeOpts maybeConfig envvars TestCliOpts{..} = do
  baseDir <- collectBaseDir testBaseDir
  apiOpts <- collectApiOpts maybeConfig envvars globals
  let timeout = TimeoutSeconds testTimeout
  revision <-
    collectRevisionData baseDir maybeConfig $
      OverrideProject (optProjectName globals) (optProjectRevision globals) Nothing
  runValidation $
    TestConfig
      <$> baseDir
      <*> apiOpts
      <*> pure timeout
      <*> pure testOutputType
      <*> revision

collectApiOpts :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> GlobalOpts -> m (Validator ApiOpts)
collectApiOpts maybeconfig envvars globals = validationBoundary $ do
  apikey <- validateApiKey maybeconfig envvars globals
  let baseuri = optBaseUrl globals
  pure $ ApiOpts baseuri apikey
