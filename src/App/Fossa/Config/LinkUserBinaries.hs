{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.LinkUserBinaries (
  mkSubCommand,
  LinkUserBinsConfig (..),
  LinkUserBinsOpts,
) where

import App.Fossa.DebugDir (DebugDirRef)
import App.Fossa.Config.Common (
  CommonOpts (..),
  collectApiOpts,
  collectBaseDir,
  commonOpts,
 )
import App.Fossa.Config.ConfigFile (ConfigFile, resolveLocalConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (
  EffStack,
  GetCommonOpts (getCommonOpts),
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
import Control.Effect.Lift (Lift)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Options.Applicative (
  InfoMod,
  Parser,
  argument,
  helpDoc,
  long,
  metavar,
  optional,
  progDescDoc,
  str,
  strOption,
  value,
 )
import Prettyprinter (Doc, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Style (applyFossaStyle, boldItalicized, formatDoc, formatStringToDoc, stringToHelpDoc)

cmdName :: String
cmdName = "experimental-link-user-defined-dependency-binary"

linkInfo :: InfoMod a
linkInfo = progDescDoc $ formatStringToDoc "Link one or more binary fingerprints as a user-defined dependency"

mkSubCommand :: (LinkUserBinsConfig -> EffStack ()) -> SubCommand LinkUserBinsOpts LinkUserBinsConfig
mkSubCommand = SubCommand cmdName linkInfo cliParser loadConfig mergeOpts

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Maybe FilePath ->
  Maybe ConfigFile ->
  EnvVars ->
  LinkUserBinsOpts ->
  m LinkUserBinsConfig
mergeOpts _ cfgfile envvars LinkUserBinsOpts{..} = do
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
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LinkUserBinsConfig where
  toEncoding = genericToEncoding defaultOptions

loadConfig ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  LinkUserBinsOpts ->
  m (Maybe ConfigFile)
loadConfig = resolveLocalConfigFile . optConfig . commons

data LinkUserBinsOpts = LinkUserBinsOpts
  { commons :: CommonOpts
  , assertionDir :: FilePath
  , assertionMeta :: UserDefinedAssertionMeta
  }
  deriving (Eq, Ord, Show)

instance GetSeverity LinkUserBinsOpts where
  getSeverity LinkUserBinsOpts{commons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

instance GetCommonOpts LinkUserBinsOpts where
  getCommonOpts LinkUserBinsOpts{commons} = Just commons

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
        <$> (strOption (applyFossaStyle <> long "name" <> stringToHelpDoc "The name to display for the dependency"))
        <*> (strOption (applyFossaStyle <> long "version" <> stringToHelpDoc "The version to display for the dependency"))
        <*> (strOption (applyFossaStyle <> long "license" <> stringToHelpDoc "The license identifier to use for the dependency"))
        <*> optional (strOption (applyFossaStyle <> long "description" <> stringToHelpDoc "The description to use for the dependency"))
        <*> optional (strOption (applyFossaStyle <> long "homepage" <> stringToHelpDoc "The URL to the homepage for the dependency"))
    assertUserDefinedBinariesDir :: Parser String
    assertUserDefinedBinariesDir = argument str (applyFossaStyle <> metavar "DIR" <> helpDoc dirHelp <> value ".")
    dirHelp :: Maybe (Doc AnsiStyle)
    dirHelp =
      Just . formatDoc $
        vsep
          [ "The directory containing one or more binaries to assert to the provided values"
          , boldItalicized "Default: " <> "Current directory"
          ]
