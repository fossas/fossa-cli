{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Container.Test (
  ContainerTestConfig (..),
  ContainerTestOptions (..),
  TestOutputFormat (..),
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
  collectArch,
  collectDockerHost,
  imageTextArg,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.Test (TestOutputFormat (TestOutputJson, TestOutputPretty), defaultOutputFmt, testOutputFormatList, validateOutputFormat)
import App.Types (OverrideProject (OverrideProject))
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Monad (when)
import Control.Timeout (Duration (Seconds))
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import Effect.Logger (Logger, logWarn, vsep)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  auto,
  command,
  flag,
  help,
  info,
  internal,
  long,
  option,
  optional,
  progDesc,
  strOption,
 )

subcommand :: (ContainerTestOptions -> a) -> Mod CommandFields a
subcommand f =
  command
    "test"
    ( info (f <$> cliParser) $
        progDesc "Check for issues from FOSSA and exit non-zero when issues are found"
    )

data ContainerTestConfig = ContainerTestConfig
  { apiOpts :: ApiOpts
  , timeoutDuration :: Duration
  , outputFormat :: TestOutputFormat
  , testImageLocator :: ImageText
  , testRevisionOverride :: OverrideProject
  , testDockerHost :: Text
  , testArch :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ContainerTestConfig where
  toEncoding = genericToEncoding defaultOptions

data ContainerTestOptions = ContainerTestOptions
  { testCommons :: CommonOpts
  , containerTestTimeout :: Maybe Int
  , deprecatedContainerTestOutputType :: TestOutputFormat
  , containerTestOutputFmt :: Maybe String
  , containerTestImage :: ImageText
  }

cliParser :: Parser ContainerTestOptions
cliParser =
  ContainerTestOptions
    <$> commonOpts
    <*> optional (option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)"))
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json" <> internal)
    <*> optional (strOption (long "format" <> help ("Output the report in the specified format. Currently available formats: (" <> testOutputFormatList <> ")")))
    <*> imageTextArg

mergeOpts ::
  (Has Diagnostics sig m, Has Logger sig m) =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerTestOptions ->
  m ContainerTestConfig
mergeOpts cfgfile envvars ContainerTestOptions{..} = do
  let apiopts = collectApiOpts cfgfile envvars testCommons
      timeout = maybe defaultTimeoutDuration Seconds containerTestTimeout
      arch = collectArch
      revOverride =
        collectRevisionOverride cfgfile $
          OverrideProject (optProjectName testCommons) (optProjectRevision testCommons) Nothing

  -- if the non-default value is present for flag, user is using deprecatedJsonFlag
  when (deprecatedContainerTestOutputType /= defaultOutputFmt) $ do
    logWarn $
      vsep
        [ "DEPRECATION NOTICE"
        , "========================"
        , "--json flag is now deprecated for `fossa container test` command."
        , ""
        , "Please use: "
        , "   `--format json` instead."
        , ""
        , "In future, usage of --json may result in fatal error."
        ]

  testOutputFormat <-
    validateOutputFormat
      (deprecatedContainerTestOutputType == TestOutputJson)
      containerTestOutputFmt

  ContainerTestConfig
    <$> apiopts
    <*> pure timeout
    <*> pure testOutputFormat
    <*> pure containerTestImage
    <*> pure revOverride
    <*> collectDockerHost envvars
    <*> pure arch
