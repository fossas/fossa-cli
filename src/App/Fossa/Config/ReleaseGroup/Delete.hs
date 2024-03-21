{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.ReleaseGroup.Delete (
  DeleteConfig (..),
  DeleteOpts (..),
  subcommand,
  cliParser,
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

deleteInfo :: InfoMod a
deleteInfo = progDescDoc $ formatStringToDoc "Delete FOSSA release groups"

subcommand :: (DeleteOpts -> a) -> Mod CommandFields a
subcommand f = command "delete" $ info (f <$> cliParser) deleteInfo

data DeleteConfig = DeleteConfig
  { apiOpts :: ApiOpts
  , releaseGroupTitle :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON DeleteConfig where
  toEncoding = genericToEncoding defaultOptions

data DeleteOpts = DeleteOpts
  { releaseGroupCommon :: ReleaseGroupCommonOpts
  , releaseGroupTitleOpts :: Text
  }
  deriving (Eq, Ord, Show, Generic)

cliParser :: Parser DeleteOpts
cliParser =
  DeleteOpts
    <$> releaseGroupCommonOpts
    <*> strOption (applyFossaStyle <> long "title" <> short 't' <> stringToHelpDoc "The title of the FOSSA release group")

mergeOpts ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  DeleteOpts ->
  m DeleteConfig
mergeOpts maybeConfig envVars DeleteOpts{..} = do
  apiOpts <- collectApiOpts maybeConfig envVars releaseGroupCommon
  pure $ DeleteConfig apiOpts releaseGroupTitleOpts
