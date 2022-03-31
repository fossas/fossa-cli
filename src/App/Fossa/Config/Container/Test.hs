{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Container.Test (
  ContainerTestConfig (..),
  ContainerTestOptions (..),
  OutputFormat (..),
  cliParser,
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (
  CommonOpts (optProjectName, optProjectRevision),
  collectApiOpts,
  collectRevisionOverride,
  commonOpts,
  defaultTimeoutDuration,
 )
import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.Container.Common (
  ImageText,
  imageTextArg,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Types (OverrideProject (OverrideProject))
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Timeout (Duration (Seconds))
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

subcommand :: (ContainerTestOptions -> a) -> Mod CommandFields a
subcommand f =
  command
    "test"
    ( info (f <$> cliParser) $
        progDesc "Check for issues from FOSSA and exit non-zero when issues are found"
    )

data OutputFormat
  = TestOutputPretty
  | TestOutputJson
  deriving (Eq, Ord, Show)

data ContainerTestConfig = ContainerTestConfig
  { apiOpts :: ApiOpts
  , timeoutDuration :: Duration
  , outputFormat :: OutputFormat
  , testImageLocator :: ImageText
  , testRevisionOverride :: OverrideProject
  }
  deriving (Eq, Ord, Show)

data ContainerTestOptions = ContainerTestOptions
  { testCommons :: CommonOpts
  , containerTestTimeout :: Maybe Int
  , containerTestOutputType :: OutputFormat
  , containerTestImage :: ImageText
  }

cliParser :: Parser ContainerTestOptions
cliParser =
  ContainerTestOptions
    <$> commonOpts
    <*> optional (option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)"))
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json")
    <*> imageTextArg

mergeOpts ::
  Has Diagnostics sig m =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerTestOptions ->
  m ContainerTestConfig
mergeOpts cfgfile envvars ContainerTestOptions{..} = do
  let apiopts = collectApiOpts cfgfile envvars testCommons
      timeout = maybe defaultTimeoutDuration Seconds containerTestTimeout
      revOverride =
        collectRevisionOverride cfgfile $
          OverrideProject (optProjectName testCommons) (optProjectRevision testCommons) Nothing
  ContainerTestConfig
    <$> apiopts
    <*> pure timeout
    <*> pure containerTestOutputType
    <*> pure containerTestImage
    <*> pure revOverride
