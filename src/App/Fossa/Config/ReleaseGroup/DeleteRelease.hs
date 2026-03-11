{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.ReleaseGroup.DeleteRelease (
  DeleteReleaseConfig (..),
  DeleteReleaseOpts (..),
  subcommand,
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
  { releaseGroupCommon :: Common.ReleaseGroupCommonOpts
  , releaseGroupTitleOpts :: Text
  , releaseGroupReleaseTitleOpts :: Text
  }
  deriving (Eq, Ord, Show, Generic)

cliParser :: Parser DeleteReleaseOpts
cliParser =
  DeleteReleaseOpts
    <$> Common.releaseGroupCommonOpts
    -- .fossa.yml configurations are disabled for release group delete commands so that lingering configurations are
    -- not extracted and used to mistakenly delete release groups or release group releases.
    <*> Common.releaseGroupTitleOpts
    <*> Common.releaseGroupReleaseTitleOpts

mergeOpts ::
  (Has Diagnostics sig m) =>
  Maybe FilePath ->
  Maybe ConfigFile ->
  EnvVars ->
  DeleteReleaseOpts ->
  m DeleteReleaseConfig
mergeOpts _ maybeConfig envVars DeleteReleaseOpts{..} = do
  apiOpts <- Common.collectApiOpts maybeConfig envVars releaseGroupCommon
  pure $ DeleteReleaseConfig apiOpts releaseGroupTitleOpts releaseGroupReleaseTitleOpts
