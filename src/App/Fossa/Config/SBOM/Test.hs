{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.SBOM.Test (
  SBOMTestConfig (..),
  SBOMTestOptions (..),
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
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.SBOM.Common (
  ImageText,
  imageTextArg,
 )
import App.Fossa.Config.Test (TestOutputFormat (TestOutputJson, TestOutputPretty), testFormatHelp, validateOutputFormat)
import App.Types (OverrideProject (OverrideProject))
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Timeout (Duration (Seconds))
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  auto,
  command,
  info,
  long,
  option,
  optional,
  progDescDoc,
  strOption,
 )
import Options.Applicative.Builder (helpDoc)
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)

subcommand :: (SBOMTestOptions -> a) -> Mod CommandFields a
subcommand f =
  command
    "test"
    ( info (f <$> cliParser) $
        progDescDoc $
          formatStringToDoc "Check for issues from FOSSA and exit non-zero when issues are found"
    )

data SBOMTestConfig = SBOMTestConfig
  { apiOpts :: ApiOpts
  , timeoutDuration :: Duration
  , outputFormat :: TestOutputFormat
  , testImageLocator :: ImageText
  , testRevisionOverride :: OverrideProject
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SBOMTestConfig where
  toEncoding = genericToEncoding defaultOptions

data SBOMTestOptions = SBOMTestOptions
  { testCommons :: CommonOpts
  , containerTestTimeout :: Maybe Int
  , containerTestOutputFmt :: Maybe String
  , containerTestImage :: ImageText
  }

cliParser :: Parser SBOMTestOptions
cliParser =
  SBOMTestOptions
    <$> commonOpts
    <*> optional (option auto (applyFossaStyle <> long "timeout" <> stringToHelpDoc "Duration to wait for build completion (in seconds)"))
    <*> optional (strOption (applyFossaStyle <> long "format" <> helpDoc testFormatHelp))
    <*> imageTextArg

mergeOpts ::
  (Has Diagnostics sig m) =>
  Maybe ConfigFile ->
  EnvVars ->
  SBOMTestOptions ->
  m SBOMTestConfig
mergeOpts cfgfile envvars SBOMTestOptions{..} = do
  let apiopts = collectApiOpts cfgfile envvars testCommons
      timeout = maybe defaultTimeoutDuration Seconds containerTestTimeout
      revOverride =
        collectRevisionOverride cfgfile $
          OverrideProject (optProjectName testCommons) (optProjectRevision testCommons) Nothing

  -- if the non-default value is present for flag, user is using deprecatedJsonFlag

  testOutputFormat <-
    validateOutputFormat
      (False)
      containerTestOutputFmt

  SBOMTestConfig
    <$> apiopts
    <*> pure timeout
    <*> pure testOutputFormat
    <*> pure containerTestImage
    <*> pure revOverride
