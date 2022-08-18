module App.Fossa.Config.Container.ListTargets (subcommand) where

import App.Fossa.Config.Container.Analyze (
  ContainerAnalyzeOptions,
  cliParser,
 )
import Options.Applicative (
  CommandFields,
  Mod,
  command,
  info,
  progDesc,
 )

subcommand :: (ContainerAnalyzeOptions -> a) -> Mod CommandFields a
subcommand f =
  command
    "list-targets"
    ( info (f <$> cliParser) $
        progDesc "Lists target with container image"
    )
