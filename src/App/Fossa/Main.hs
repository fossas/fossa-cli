{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Main (appMain) where

import App.Fossa.Analyze qualified as Analyze
import App.Fossa.Container qualified as Container
import App.Fossa.DumpBinaries qualified as Dump
import App.Fossa.Init (initCommand)
import App.Fossa.LicenseScan qualified as LicenseScan (licenseScanSubCommand)
import App.Fossa.ListTargets qualified as ListTargets
import App.Fossa.Report qualified as Report
import App.Fossa.Snippets qualified as Snippets
import App.Fossa.Subcommand (GetCommonOpts, GetSeverity, SubCommand (..), runSubCommand)
import App.Fossa.Test qualified as Test
import App.Fossa.VSI.IAT.AssertUserDefinedBinaries qualified as LinkBins
import App.Support (supportUrl)
import App.Version (fullVersionDescription)
import Control.Concurrent.CGroup (initRTSThreads)
import Control.Monad (join)
import Data.Aeson (ToJSON)
import Data.String.Conversion (toString)
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import Effect.Logger (indent, newlinePreceding, newlineTrailing, pretty, vsep)
import Options.Applicative (
  CommandFields,
  InfoMod,
  Mod,
  Parser,
  ParserPrefs,
  columns,
  command,
  customExecParser,
  footer,
  fullDesc,
  header,
  helpIndent,
  helpShowGlobals,
  info,
  infoOption,
  internal,
  long,
  prefs,
  progDescDoc,
  short,
  showHelpOnEmpty,
  showHelpOnError,
  subparser,
  subparserInline,
  (<**>),
  (<|>),
 )
import Options.Applicative.Extra (helperWith)
import Style (applyFossaStyle, formatDoc, formatStringToDoc, stringToHelpDoc)

appMain :: IO ()
appMain = do
  initRTSThreads
  join $ customExecParser mainPrefs $ info (subcommands <**> helperOpt <**> versionOpt) progData

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption (toString fullVersionDescription) $
    mconcat
      [applyFossaStyle, long "version", short 'V', stringToHelpDoc "Show version information and exit"]

helperOpt :: Parser (a -> a)
helperOpt =
  helperWith
    ( mconcat
        [ applyFossaStyle
        , long "help"
        , short 'h'
        , stringToHelpDoc "Show this help text"
        ]
    )

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
          , experimentalLicenseScanCommand
          , decodeSubCommand Dump.dumpSubCommand
          , decodeSubCommand LicenseScan.licenseScanSubCommand
          ]
    public =
      subparser $
        mconcat
          [ decodeSubCommand Analyze.analyzeSubCommand
          , decodeSubCommand Test.testSubCommand
          , decodeSubCommand Report.reportSubCommand
          , decodeSubCommand Container.containerSubCommand
          , decodeSubCommand ListTargets.listSubCommand
          , decodeSubCommand LinkBins.linkBinsSubCommand
          , decodeSubCommand Snippets.snippetsSubCommand
          , initCommand
          , feedbackCommand
          ]

experimentalLicenseScanCommand :: Mod CommandFields (IO ())
experimentalLicenseScanCommand = command "experimental-license-scan" (info runInit $ progDescDoc $ formatStringToDoc "The 'experimental-license-scan' command has been deprecated and renamed to 'license-scan'")
  where
    runInit :: Parser (IO ())
    runInit = pure $ putStrLn "The 'experimental-license-scan' has been deprecated and renamed to 'license-scan'. Please use the 'license-scan' command instead."

feedbackCommand :: Mod CommandFields (IO ())
feedbackCommand = command "feedback" (info feedbackPrompt $ progDescDoc $ formatStringToDoc "Provide feedback on your fossa-cli experience, submit feature requests, and report bugs/issues")
  where
    feedbackPrompt :: Parser (IO ())
    feedbackPrompt =
      pure $
        putDoc $
          vsep
            [ newlinePreceding "At FOSSA, we are committed to delivering an exceptional user experience and are continously working towards improving our product."
            , "Your feedback is crucial in shaping our ongoing efforts to innovate and provide an even better user experience!"
            , ""
            , "* Report bugs and issues at:"
            , newlineTrailing . newlinePreceding $ indent 4 $ pretty supportUrl
            , "* Submit feature requests to:"
            , newlineTrailing . newlinePreceding $ indent 4 "support@fossa.com"
            , "* Provide feedback on overall cli experience at:"
            , newlineTrailing . newlineTrailing . newlinePreceding $ indent 4 "https://docs.google.com/forms/d/e/1FAIpQLSdfz4KX_j6lpCF0zICQaUd-Rjn_Vj-IelCSmqOii__fqyYVmg/viewform"
            ]

-- putStrLn "At FOSSA, we are committed to delivering an exceptional user experience and are continously working towards improving our product."
-- putStrLn "Your feedback is crucial in shaping our ongoing efforts to innovate and provide an even better user experience!"
-- putStrLn ("\n * Report bugs and issues at: " <> toString supportUrl)
-- putStrLn ("\n * Submit feature requests to: support@fossa.com")
-- putStrLn ("\n * Provide feedback on overall cli experience at: https://docs.google.com/forms/d/e/1FAIpQLSdfz4KX_j6lpCF0zICQaUd-Rjn_Vj-IelCSmqOii__fqyYVmg/viewform")

decodeSubCommand :: (GetSeverity a, GetCommonOpts a, Show b, ToJSON b) => SubCommand a b -> Mod CommandFields (IO ())
decodeSubCommand cmd@SubCommand{..} = command commandName $ info (runSubCommand cmd) commandInfo

mainPrefs :: ParserPrefs
mainPrefs =
  prefs $
    mconcat
      [ showHelpOnEmpty
      , showHelpOnError
      , subparserInline
      , columns 150
      , helpShowGlobals
      , helpIndent 1 -- allows the help message to appear on new line
      ]
