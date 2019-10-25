
module App
  ( appMain
  ) where

import Prologue

import Options.Applicative
import Path.IO
import System.Exit (die)

import App.Scan (scanMain)

appMain :: IO ()
appMain = do
  options <- execParser opts

  currentDir <- getCurrentDir

  case options of
    ScanCmd basedir ->
      case basedir of
        Nothing -> scanMain currentDir
        Just dir -> do
          resolved <- validateDir dir
          scanMain resolved

newtype CommandOpts = ScanCmd (Maybe FilePath) -- basedir for scanning
  deriving Show

validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute

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
    optional (strOption $ long "basedir" <> short 'd' <> metavar "DIR" <> help "Base directory for scanning"))
  (progDesc "Scan for dependencies")
