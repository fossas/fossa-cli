module App.VPSScan.Main
  ( appMain
  ) where

import Prologue

import Options.Applicative

import App.OptionExtensions
import App.VPSScan.Scan (ScanCmdOpts(..), scanMain)
import App.VPSScan.NinjaGraph (NinjaGraphCmdOpts(..), ninjaGraphMain)
import App.VPSScan.Types

appMain :: IO ()
appMain = join (customExecParser (prefs showHelpOnEmpty) opts)

opts :: ParserInfo (IO ())
opts = info (commands <**> helper) (fullDesc <> header "vpscli -- FOSSA Vendored Package Scan CLI")

commands :: Parser (IO ())
commands = hsubparser $ scanCommand <> ninjaGraphCommand

vpsOpts :: Parser VPSOpts
vpsOpts = VPSOpts <$> fossaOpts <*> projectNameOpt <*> projectRevision <*> skipIprScanOpt <*> filterOpt
  where
    projectNameOpt = strOption (long "project-name" <> metavar "STRING" <> help "The name of the project to create in FOSSA")
    projectRevision = optional (strOption (long "project-revision" <> metavar "STRING" <> help "The revision of the project to create in FOSSA. If not specified, uses the current Unix timestamp."))
    skipIprScanOpt = switch (long "skip-ipr-scan" <> help "If specified, the scan directory will not be scanned for intellectual property rights information")

ninjaGraphOpts :: Parser NinjaGraphOpts
ninjaGraphOpts = NinjaGraphOpts <$> ninjaDepsOpt <*> lunchTargetOpt <*> scotlandYardUrlOpt
  where
    ninjaDepsOpt = optional $ strOption (long "ninjadeps" <> metavar "STRING")
    lunchTargetOpt = optional $ strOption (long "lunchtarget" <> metavar "STRING" <> help "build target name to pass to lunch. If you are running in an environment with envsetup and lunch already configured, then you don't need to pass this in")
    scotlandYardUrlOpt = uriOption (long "scotland-yard-url" <> metavar "STRING" <> help "URL for Scotland Yard service")

fossaOpts :: Parser FossaOpts
fossaOpts = FossaOpts <$> urlOpt <*> apiKeyOpt
  where
    urlOpt = uriOption (long "fossa-url" <> metavar "STRING" <> help "URL for FOSSA service")
    apiKeyOpt =  strOption (long "fossa-api-key" <> metavar "STRING" <> help "API key for FOSSA service")

basedirOpt :: Parser FilePath
basedirOpt = strOption (long "basedir" <> short 'd' <> metavar "DIR" <> help "Base directory for scanning" <> value ".")

filterOpt :: Parser FilterExpressions
filterOpt = FilterExpressions <$> strOption (long "ignore-file-regex" <> short 'i' <> metavar "REGEXPS" <> help "JSON encoded array of regular expressions used to filter scanned paths" <> value "[]")

scanCommand :: Mod CommandFields (IO ())
scanCommand = command "scan" (info (scanMain <$> scanOptsParser) (progDesc "Scan for projects and their dependencies"))
  where
    scanOptsParser = ScanCmdOpts <$> basedirOpt <*> vpsOpts

ninjaGraphCommand :: Mod CommandFields (IO ())
ninjaGraphCommand = command "ninja-graph" (info (ninjaGraphMain <$> ninjaGraphOptsParser) (progDesc "Get a dependency graph for a ninja build"))
  where
    ninjaGraphOptsParser = NinjaGraphCmdOpts <$> basedirOpt <*> ninjaGraphOpts
