module App.Fossa.Config.Container.Parse (
  ContainerParseFileConfig (..),
  mergeOpts,
  cliParser,
  subcommand,
) where

import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import Control.Effect.Lift (Has, Lift, sendIO)
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
import Path.IO (getCurrentDir, resolveFile)

subcommand :: (FilePath -> a) -> Mod CommandFields a
subcommand f =
  command
    "parse-file"
    ( info (f <$> cliParser) $
        progDesc "Debug syft output parsing"
    )

mergeOpts :: Has (Lift IO) sig m => Maybe ConfigFile -> EnvVars -> FilePath -> m ContainerParseFileConfig
mergeOpts _ _ fp = do
  curdir <- sendIO getCurrentDir
  path <- sendIO $ resolveFile curdir fp
  pure $ ContainerParseFileConfig path

cliParser :: Parser FilePath
cliParser = argument str (metavar "FILE" <> help "File to parse")

newtype ContainerParseFileConfig = ContainerParseFileConfig
  { sourceFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)
