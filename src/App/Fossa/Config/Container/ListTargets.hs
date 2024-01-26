{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Container.ListTargets (
  subcommand,
  mergeOpts,
  ContainerListTargetsOptions (..),
  ContainerListTargetsConfig (..),
) where

import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.Container.Common (
  ImageText,
  collectArch,
  collectDockerHost,
  imageTextArg,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding)
import Data.Aeson.Types (toEncoding)
import Data.Text
import GHC.Generics (Generic)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  command,
  info,
  progDescDoc,
 )
import Style (formatStringToDoc)

newtype ContainerListTargetsOptions = ContainerListTargetsOptions
  { imageLocator :: ImageText
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ContainerListTargetsOptions where
  toEncoding = genericToEncoding defaultOptions

data ContainerListTargetsConfig = ContainerListTargetsConfig
  { cfgImageLocator :: ImageText
  , dockerHost :: Text
  , arch :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ContainerListTargetsConfig where
  toEncoding = genericToEncoding defaultOptions

subcommand :: (ContainerListTargetsOptions -> a) -> Mod CommandFields a
subcommand f =
  command
    "list-targets"
    ( info (f <$> listTargetParser) $
        progDescDoc $
          formatStringToDoc "Lists target with container image"
    )

listTargetParser :: Parser ContainerListTargetsOptions
listTargetParser =
  ContainerListTargetsOptions
    <$> imageTextArg

mergeOpts ::
  Has Diagnostics sig m =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerListTargetsOptions ->
  m ContainerListTargetsConfig
mergeOpts _ envvars ContainerListTargetsOptions{..} =
  ContainerListTargetsConfig imageLocator
    <$> collectDockerHost envvars
    <*> pure (collectArch)
