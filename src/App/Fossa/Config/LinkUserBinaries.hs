{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.LinkUserBinaries (
  mkSubCommand,
  LinkUserBinsConfig (..),
  LinkUserBinsOpts,
) where

import App.Fossa.Config.Common (
  CommonOpts (..),
  collectApiOpts,
  collectBaseDir,
  commonOpts,
 )
import App.Fossa.Config.ConfigFile (ConfigFile, resolveConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (
  EffStack,
  GetSeverity (..),
  SubCommand (SubCommand),
 )
import App.Fossa.VSI.IAT.Types (
  UserDefinedAssertionMeta (UserDefinedAssertionMeta),
 )
import App.Types (BaseDir)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
 )
import Control.Effect.Lift (Lift, sendIO)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Options.Applicative (
  InfoMod,
  Parser,
  argument,
  help,
  long,
  metavar,
  optional,
  progDesc,
  str,
  strOption,
  value,
 )
import Path.IO (getCurrentDir)

cmdName :: String
cmdName = "experimental-link-user-defined-dependency-binary"

linkInfo :: InfoMod a
linkInfo = progDesc "Link one or more binary fingerprints as a user-defined dependency"

mkSubCommand :: (LinkUserBinsConfig -> EffStack ()) -> SubCommand LinkUserBinsOpts LinkUserBinsConfig
mkSubCommand = SubCommand cmdName linkInfo cliParser loadConfig mergeOpts

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  LinkUserBinsOpts ->
  m LinkUserBinsConfig
mergeOpts cfgfile envvars LinkUserBinsOpts{..} = do
  let apiopts = collectApiOpts cfgfile envvars commons
      basedir = collectBaseDir assertionDir
      metadata = assertionMeta
  LinkUserBinsConfig
    <$> apiopts
    <*> basedir
    <*> pure metadata

data LinkUserBinsConfig = LinkUserBinsConfig
  { apiOpts :: ApiOpts
  , baseDir :: BaseDir
  , binMetadata :: UserDefinedAssertionMeta
  }
  deriving (Eq, Ord, Show)

loadConfig ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  LinkUserBinsOpts ->
  m (Maybe ConfigFile)
loadConfig LinkUserBinsOpts{..} = do
  curdir <- sendIO getCurrentDir
  resolveConfigFile curdir $ optConfig commons

data LinkUserBinsOpts = LinkUserBinsOpts
  { commons :: CommonOpts
  , assertionDir :: FilePath
  , assertionMeta :: UserDefinedAssertionMeta
  }
  deriving (Eq, Ord, Show)

instance GetSeverity LinkUserBinsOpts where
  getSeverity LinkUserBinsOpts{commons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

cliParser :: Parser LinkUserBinsOpts
cliParser =
  LinkUserBinsOpts
    <$> commonOpts
    <*> assertUserDefinedBinariesDir
    <*> assertUserDefinedBinariesMeta
  where
    assertUserDefinedBinariesMeta :: Parser UserDefinedAssertionMeta
    assertUserDefinedBinariesMeta =
      UserDefinedAssertionMeta
        <$> (strOption (long "name" <> help "The name to display for the dependency"))
        <*> (strOption (long "version" <> help "The version to display for the dependency"))
        <*> (strOption (long "license" <> help "The license identifier to use for the dependency"))
        <*> optional (strOption (long "description" <> help "The description to use for the dependency"))
        <*> optional (strOption (long "homepage" <> help "The URL to the homepage for the dependency"))
    assertUserDefinedBinariesDir :: Parser String
    assertUserDefinedBinariesDir = argument str (metavar "DIR" <> help "The directory containing one or more binaries to assert to the provided values (default: current directory)" <> value ".")
