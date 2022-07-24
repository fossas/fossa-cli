module App.Pathfinder.Main (
  appMain,
) where

import App.Pathfinder.Scan (scanMain)
import App.Types (BaseDir (..))
import App.Util (validateDir)
import Control.Concurrent.CGroup (initRTSThreads)
import Data.Maybe (fromMaybe)
import Options.Applicative
import Path.IO

appMain :: IO ()
appMain = do
  initRTSThreads
  options <- execParser opts

  currentDir <- getCurrentDir

  case options of
    LicenseScan basedir debug ->
      case basedir of
        Nothing -> scanMain currentDir debug
        Just dir -> do
          resolved <- validateDir dir
          scanMain (unBaseDir resolved) debug

data CommandOpts = LicenseScan (Maybe FilePath) Bool
  deriving (Show)

opts :: ParserInfo CommandOpts
opts =
  info
    (commands <**> helper)
    (fullDesc <> header "pathfinder - finds declared licenses in manifest files")

commands :: Parser CommandOpts
commands =
  hsubparser
    ( command "scan" scanCmd
    -- <> command "other" -- etc
    )

scanCmd :: ParserInfo CommandOpts
scanCmd =
  info
    ( LicenseScan
        <$> optional (strOption $ long "basedir" <> short 'd' <> metavar "DIR" <> help "Base directory for scanning")
        <*> (fromMaybe False <$> optional (switch $ long "debug" <> help "Enable debug logging"))
    )
    (progDesc "Scan for licenses")
