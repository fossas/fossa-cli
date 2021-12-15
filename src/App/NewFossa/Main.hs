{-# LANGUAGE RecordWildCards #-}

module App.NewFossa.Main (appMain) where

import App.Fossa.Analyze qualified as Analyze
import App.Fossa.Test qualified as Test
import App.NewFossa.Subcommand (GetSeverity, SubCommand (..), runSubCommand)
import App.Version (fullVersionDescription)
import Control.Monad (join)
import Data.String.Conversion (toString)
import Options.Applicative (
  CommandFields,
  InfoMod,
  Mod,
  Parser,
  ParserPrefs,
  command,
  customExecParser,
  fullDesc,
  header,
  help,
  helpShowGlobals,
  helper,
  hidden,
  hsubparser,
  info,
  infoOption,
  long,
  prefs,
  short,
  showHelpOnEmpty,
  showHelpOnError,
  subparserInline,
  (<**>),
 )

appMain :: IO ()
appMain = join $ customExecParser mainPrefs $ info (subcommands <**> helper <**> versionOpt) progData

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption (toString fullVersionDescription) $
    mconcat
      [long "version", short 'V', help "show version information", hidden]

progData :: InfoMod (IO ())
progData = fullDesc <> header "fossa-cli - Flexible, performant dependency analysis"

subcommands :: Parser (IO ())
subcommands =
  hsubparser $
    mconcat
      [ decodeSubCommand Analyze.analyzeSubCommand
      , decodeSubCommand Test.testSubCommand
      ]

decodeSubCommand :: GetSeverity a => SubCommand a b -> Mod CommandFields (IO ())
decodeSubCommand cmd@SubCommand{..} = command commandName $ info (runSubCommand cmd) commandInfo

mainPrefs :: ParserPrefs
mainPrefs =
  prefs $
    mconcat
      [ showHelpOnEmpty
      , showHelpOnError
      , subparserInline
      , helpShowGlobals
      ]
