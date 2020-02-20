
module App
  ( appMain
  ) where

import Prologue

import Options.Applicative

import App.Scan (ScanCmdOpts(..), scanMain)

appMain :: IO ()
appMain = join (customExecParser (prefs showHelpOnEmpty) opts)

opts :: ParserInfo (IO ())
opts = info (commands <**> helper) (fullDesc <> header "hscli - fossa-cli, but functional")

commands :: Parser (IO ())
commands = hsubparser scanCommand

scanCommand :: Mod CommandFields (IO ())
scanCommand = command "scan" (info (scanMain <$> scanOptsParser) (progDesc "Scan for projects and their dependencies"))
  where
  scanOptsParser = ScanCmdOpts <$> basedirOpt <*> debugOpt <*> outputOpt

  basedirOpt = strOption (long "basedir" <> short 'd' <> metavar "DIR" <> help "Base directory for scanning" <> value ".")
  debugOpt = switch (long "debug" <> help "Enable debug logging")
  outputOpt = optional (strOption (long "outfile" <> short 'o' <> metavar "FILE" <> help "Output results to a file (default: stdout). Relative paths are relative to the scan root."))
