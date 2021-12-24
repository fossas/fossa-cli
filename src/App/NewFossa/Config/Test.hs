{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.NewFossa.Config.Test (
  TestCliOpts,
  TestConfig (..),
  OutputFormat (..),
  mkSubCommand,
) where

import App.NewFossa.Config.Common (
  GlobalOpts (..),
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectRevisionData,
  defaultTimeoutDuration,
  globalOpts,
 )
import App.NewFossa.ConfigFile (ConfigFile, resolveConfigFile)
import App.NewFossa.EnvironmentVars (EnvVars)
import App.NewFossa.Subcommand (EffStack, GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (BaseDir, OverrideProject (OverrideProject), ProjectRevision)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  runValidation,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Timeout (Duration (..))
import Effect.Exec (Exec)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Options.Applicative (
  InfoMod,
  Parser,
  auto,
  flag,
  help,
  long,
  option,
  optional,
  progDesc,
 )
import Path.IO (getCurrentDir)

data OutputFormat
  = TestOutputPretty
  | TestOutputJson
  deriving (Eq, Ord, Show)

data TestCliOpts = TestCliOpts
  { globals :: GlobalOpts
  , testTimeout :: Maybe Int
  , testOutputType :: OutputFormat
  , testBaseDir :: FilePath
  }
  deriving (Eq, Ord, Show)

instance GetSeverity TestCliOpts where
  getSeverity TestCliOpts{globals = GlobalOpts{optDebug}} = if optDebug then SevDebug else SevInfo

data TestConfig = TestConfig
  { baseDir :: BaseDir
  , apiOpts :: ApiOpts
  , timeout :: Duration
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
    <*> optional (option auto (long "timeout" <> help "Duration to wait for build completion in seconds (Defaults to 1 hour)"))
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json")
    <*> baseDirArg

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
  let timeout = maybe defaultTimeoutDuration Seconds testTimeout
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
