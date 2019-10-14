
module App
  ( appMain
  ) where

import Prologue

import Data.Maybe (fromMaybe)
import Options.Applicative
import Path.IO

import App.Scan (scanMain)

appMain :: IO ()
appMain = do
  options <- execParser opts

  currentDir <- getCurrentDir

  case options of
    ScanCmd basedir -> scanMain (fromMaybe currentDir basedir)

newtype CommandOpts = ScanCmd (Maybe (Path Abs Dir)) -- basedir for scanning
  deriving Show

opts :: ParserInfo CommandOpts
opts = info (commands <**> helper)
  ( fullDesc <> header "hscli - fossa-cli, but functional")

commands :: Parser CommandOpts
commands = hsubparser
  ( command "scan" scanCmd
  -- <> command "other" -- etc
  )

scanCmd :: ParserInfo CommandOpts
scanCmd = info
  (ScanCmd <$>
    optional (option (maybeReader parseAbsDir) $ long "basedir" <> short 'd' <> metavar "DIR" <> help "Base directory for scanning"))
  (progDesc "Scan for dependencies")
