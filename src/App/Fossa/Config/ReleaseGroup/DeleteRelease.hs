{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.ReleaseGroup.DeleteRelease (
  DeleteReleaseConfig (..),
  DeleteReleaseOpts (..),
  subcommand,
  mergeOpts,
) where

import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.ReleaseGroup.Common (ReleaseGroupCommonOpts (..), collectApiOpts, releaseGroupCommonOpts)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Text (Text)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Options.Applicative (
  CommandFields,
  InfoMod,
  Mod,
  Parser,
  command,
  info,
  long,
  short,
  strOption,
 )
import Options.Applicative.Builder (progDescDoc)
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)

deleteReleaseInfo :: InfoMod a
deleteReleaseInfo = progDescDoc $ formatStringToDoc "Delete a FOSSA release group release"

subcommand :: (DeleteReleaseOpts -> a) -> Mod CommandFields a
subcommand f = command "delete-release" $ info (f <$> cliParser) deleteReleaseInfo

data DeleteReleaseConfig = DeleteReleaseConfig
  { apiOpts :: ApiOpts
  , releaseGroupTitle :: Text
  , releaseGroupReleaseTitle :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON DeleteReleaseConfig where
  toEncoding = genericToEncoding defaultOptions

data DeleteReleaseOpts = DeleteReleaseOpts
  { releaseGroupCommon :: ReleaseGroupCommonOpts
  , releaseGroupTitleOpts :: Text
  , releaseGroupReleaseTitleOpts :: Text
  }
  deriving (Eq, Ord, Show, Generic)

cliParser :: Parser DeleteReleaseOpts
cliParser =
  DeleteReleaseOpts
    <$> releaseGroupCommonOpts
    <*> strOption (applyFossaStyle <> long "title" <> short 't' <> stringToHelpDoc "The title of the FOSSA release group")
    <*> strOption (applyFossaStyle <> long "release" <> short 'r' <> stringToHelpDoc "The release of the FOSSA release group")

mergeOpts ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  DeleteReleaseOpts ->
  m DeleteReleaseConfig
mergeOpts maybeConfig envVars DeleteReleaseOpts{..} = do
  apiOpts <- collectApiOpts maybeConfig envVars releaseGroupCommon
  pure $ DeleteReleaseConfig apiOpts releaseGroupTitleOpts releaseGroupReleaseTitleOpts
