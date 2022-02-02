{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.VPS.Test (
  TestConfig (..),
  TestOpts (..),
  OutputFormat (..),
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (
  CacheAction (ReadOnly),
  CommonOpts (optProjectName, optProjectRevision),
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectRevisionData',
  commonOpts,
  defaultTimeoutDuration,
 )
import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Types (
  BaseDir,
  OverrideProject (OverrideProject),
  ProjectRevision,
 )
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Control.Timeout (Duration (Seconds))
import Effect.Exec (Exec)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  auto,
  command,
  flag,
  help,
  info,
  long,
  option,
  optional,
  progDesc,
 )

data OutputFormat
  = TestOutputJson
  | TestOutputPretty
  deriving (Eq, Ord, Show)

data TestConfig = TestConfig
  { testApiOpts :: ApiOpts
  , testBaseDir :: BaseDir
  , testOutputFormat :: OutputFormat
  , testRevision :: ProjectRevision
  , testTimeoutDuration :: Duration
  }
  deriving (Eq, Ord, Show)

data TestOpts = TestOpts
  { testCommons :: CommonOpts
  , vpsTestTimeout :: Maybe Int
  , vpsTestOutputType :: OutputFormat
  , vpsTestBaseDir :: FilePath
  }
  deriving (Eq, Ord, Show)

mergeOpts ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  TestOpts ->
  m TestConfig
mergeOpts cfgfile envvars TestOpts{..} = do
  let outputFormat = vpsTestOutputType
      timeoutDuration = maybe defaultTimeoutDuration Seconds vpsTestTimeout
      apiopts = collectApiOpts cfgfile envvars testCommons
      basedir = collectBaseDir vpsTestBaseDir
      revision =
        collectRevisionData' basedir cfgfile ReadOnly $
          OverrideProject
            (optProjectName testCommons)
            (optProjectRevision testCommons)
            Nothing
  TestConfig
    <$> apiopts
    <*> basedir
    <*> pure outputFormat
    <*> revision
    <*> pure timeoutDuration

subcommand :: (TestOpts -> a) -> Mod CommandFields a
subcommand f =
  command
    "test"
    ( info (f <$> cliParser) $
        progDesc "Check for issues from FOSSA and exit non-zero when issues are found"
    )

cliParser :: Parser TestOpts
cliParser =
  TestOpts
    <$> commonOpts
    <*> optional (option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)"))
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json")
    <*> baseDirArg
