module App.Fossa.Config.Container.Parse (
  ContainerParseFileConfig (..),
  mergeOpts,
  cliParser,
  subcommand,
) where

import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import Effect.ReadFS (ReadFS, getCurrentDir, resolveFile)
import GHC.Generics (Generic)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  argument,
  command,
  help,
  info,
  metavar,
  progDesc,
  str,
 )
import Path (Abs, File, Path)

subcommand :: (Text -> a) -> Mod CommandFields a
subcommand f =
  command
    "parse-file"
    ( info (f <$> cliParser) $
        progDesc "Debug syft output parsing"
    )

mergeOpts ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  Text ->
  m ContainerParseFileConfig
mergeOpts _ _ fp = do
  curdir <- getCurrentDir
  path <- resolveFile curdir fp
  pure $ ContainerParseFileConfig path

cliParser :: Parser Text
cliParser = argument str (metavar "FILE" <> help "File to parse")

newtype ContainerParseFileConfig = ContainerParseFileConfig
  { sourceFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ContainerParseFileConfig where
  toEncoding = genericToEncoding defaultOptions
