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
import App.Fossa.Config.ReleaseGroup.Common qualified as Common
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
 )
import Options.Applicative.Builder (progDescDoc)
import Style (formatStringToDoc)

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
  { releaseGroupCommon :: Common.ReleaseGroupCommonOpts
  , releaseGroupTitleOpts :: Text
  }
  deriving (Eq, Ord, Show, Generic)

cliParser :: Parser DeleteOpts
cliParser =
  DeleteOpts
    <$> Common.releaseGroupCommonOpts
    -- .fossa.yml configurations are disabled for release group delete commands so that lingering configurations are
    -- not extracted and used to mistakenly delete release groups or release group releases.
    <*> Common.releaseGroupTitleOpts

mergeOpts ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  DeleteOpts ->
  m DeleteConfig
mergeOpts maybeConfig envVars DeleteOpts{..} = DeleteConfig <$> (Common.collectApiOpts maybeConfig envVars releaseGroupCommon) <*> pure releaseGroupTitleOpts
