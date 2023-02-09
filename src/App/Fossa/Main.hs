{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Main (appMain) where

import App.Fossa.Analyze qualified as Analyze
import App.Fossa.Analyze.Log4jReport qualified as Log4j
import App.Fossa.Container qualified as Container
import App.Fossa.DumpBinaries qualified as Dump
import App.Fossa.LicenseScan qualified as LicenseScan (licenseScanSubCommand)
import App.Fossa.ListTargets qualified as ListTargets
import App.Fossa.Report qualified as Report
import App.Fossa.Subcommand (GetCommonOpts, GetSeverity, SubCommand (..), runSubCommand)
import App.Fossa.Test qualified as Test
import App.Fossa.VSI.IAT.AssertUserDefinedBinaries qualified as LinkBins
import App.Version (fullVersionDescription)
import Control.Concurrent.CGroup (initRTSThreads)
import Control.Monad (join)
import Data.Aeson (ToJSON)
import Data.String.Conversion (toString)
import Options.Applicative (
  CommandFields,
  InfoMod,
  Mod,
  Parser,
  ParserPrefs,
  command,
  customExecParser,
  footer,
  fullDesc,
  header,
  help,
  helpShowGlobals,
  helper,
  hsubparser,
  info,
  infoOption,
  internal,
  long,
  prefs,
  progDesc,
  short,
  showHelpOnEmpty,
  showHelpOnError,
  subparser,
  subparserInline,
  (<**>),
  (<|>),
 )

appMain :: IO ()
appMain = do
  initRTSThreads
  join $ customExecParser mainPrefs $ info (subcommands <**> helper <**> versionOpt) progData

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption (toString fullVersionDescription) $
    mconcat
      [long "version", short 'V', help "show version information and exit"]

progData :: InfoMod (IO ())
progData =
  fullDesc
    <> header "fossa-cli - Flexible, performant dependency analysis"
    <> footer "Subcommands have additional options, run 'fossa COMMAND -h' for more details"

subcommands :: Parser (IO ())
subcommands = public <|> private
  where
    private =
      subparser $
        mconcat
          [ internal
          , initCommand
          , experimentalLicenseScanCommand
          , decodeSubCommand Dump.dumpSubCommand
          , decodeSubCommand Log4j.log4jSubCommand
          , decodeSubCommand LicenseScan.licenseScanSubCommand
          ]
    public =
      hsubparser $
        mconcat
          [ decodeSubCommand Analyze.analyzeSubCommand
          , decodeSubCommand Test.testSubCommand
          , decodeSubCommand Report.reportSubCommand
          , decodeSubCommand Container.containerSubCommand
          , decodeSubCommand ListTargets.listSubCommand
          , decodeSubCommand LinkBins.linkBinsSubCommand
          ]

initCommand :: Mod CommandFields (IO ())
initCommand = command "init" (info runInit $ progDesc "Deprecated, no longer has any effect.")
  where
    runInit :: Parser (IO ())
    runInit = pure $ putStrLn "The 'init' command has been deprecated and no longer has any effect.  You may safely remove this command."

experimentalLicenseScanCommand :: Mod CommandFields (IO ())
experimentalLicenseScanCommand = command "experimental-license-scan" (info runInit $ progDesc "The 'experimental-license-scan' command has been deprecated and renamed to 'license-scan'")
  where
    runInit :: Parser (IO ())
    runInit = pure $ putStrLn "The 'experimental-license-scan' has been deprecated and renamed to 'license-scan'. Please use the 'license-scan' command instead."

decodeSubCommand :: (GetSeverity a, GetCommonOpts a, Show b, ToJSON b) => SubCommand a b -> Mod CommandFields (IO ())
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
