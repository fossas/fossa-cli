{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Test (
  TestCliOpts,
  TestConfig (..),
  OutputFormat (..),
  mkSubCommand,
) where

import App.Fossa.Config.Common (
  CacheAction (ReadOnly),
  CommonOpts (..),
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectRevisionData',
  commonOpts,
  defaultTimeoutDuration,
 )
import App.Fossa.Config.ConfigFile (ConfigFile, resolveConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (EffStack, GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (BaseDir, OverrideProject (OverrideProject), ProjectRevision)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
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
  { commons :: CommonOpts
  , testTimeout :: Maybe Int
  , testOutputType :: OutputFormat
  , testBaseDir :: FilePath
  }
  deriving (Eq, Ord, Show)

instance GetSeverity TestCliOpts where
  getSeverity TestCliOpts{commons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

data TestConfig = TestConfig
  { baseDir :: BaseDir
  , apiOpts :: ApiOpts
  , timeout :: Duration
  , outputFormat :: OutputFormat
  , projectRevision :: ProjectRevision
  }
  deriving (Show)

testInfo :: InfoMod a
testInfo = progDesc "Check for issues from FOSSA and exit non-zero when issues are found"

mkSubCommand :: (TestConfig -> EffStack ()) -> SubCommand TestCliOpts TestConfig
mkSubCommand = SubCommand "test" testInfo parser loadConfig mergeOpts

parser :: Parser TestCliOpts
parser =
  TestCliOpts
    <$> commonOpts
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
loadConfig TestCliOpts{commons = CommonOpts{optConfig}} = do
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
  let baseDir = collectBaseDir testBaseDir
      apiOpts = collectApiOpts maybeConfig envvars commons
      timeout = maybe defaultTimeoutDuration Seconds testTimeout
      revision =
        collectRevisionData' baseDir maybeConfig ReadOnly $
          OverrideProject (optProjectName commons) (optProjectRevision commons) Nothing
  TestConfig
    <$> baseDir
    <*> apiOpts
    <*> pure timeout
    <*> pure testOutputType
    <*> revision
