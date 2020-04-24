
module App.Pathfinder.Main
  ( appMain
  ) where

import Prologue

import Options.Applicative
import Path.IO
import System.Exit (die)

import App.Pathfinder.Scan (scanMain)

appMain :: IO ()
appMain = do
  options <- execParser opts

  currentDir <- getCurrentDir

  case options of
    LicenseScan basedir debug ->
      case basedir of
        Nothing -> scanMain currentDir debug
        Just dir -> do
          resolved <- validateDir dir
          scanMain resolved debug

data CommandOpts = LicenseScan (Maybe FilePath) Bool
  deriving Show

validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute

opts :: ParserInfo CommandOpts
opts = info (commands <**> helper)
  ( fullDesc <> header "pathfinder - finds declared licenses in manifest files")

commands :: Parser CommandOpts
commands = hsubparser
  ( command "scan" scanCmd
  -- <> command "other" -- etc
  )

scanCmd :: ParserInfo CommandOpts
scanCmd = info
  (LicenseScan <$> optional (strOption $ long "basedir" <> short 'd' <> metavar "DIR" <> help "Base directory for scanning")
           <*> (fromMaybe False <$> optional (switch $ long "debug" <> help "Enable debug logging"))
  )
  (progDesc "Scan for licenses")
