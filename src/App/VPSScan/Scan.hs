module App.VPSScan.Scan
  ( scanMain
  , ScanCmdOpts(..)
  , VPSOpts(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import Path.IO
import System.Exit (exitFailure, die)
import Control.Concurrent.Async (concurrently)
import Control.Carrier.Trace.Printing
import Effect.ErrorUtils (tagError)

import Network.HTTP.Req (HttpException)

import App.VPSScan.Types
import App.VPSScan.Scan.RunSherlock
import App.VPSScan.Scan.ScotlandYard
import App.VPSScan.Scan.RunIPR

data ScanCmdOpts = ScanCmdOpts
  { cmdBasedir :: FilePath
  , scanVpsOpts :: VPSOpts
  } deriving Generic

scanMain :: ScanCmdOpts -> IO ()
scanMain opts@ScanCmdOpts{..} = do
  basedir <- validateDir cmdBasedir
  result <- runError @VPSError $ runScotlandYard $ runTrace $ runIPR $ runSherlock $ vpsScan basedir opts
  case result of
    Left err -> do
      print err
      exitFailure
    Right _ -> pure ()

----- main logic

data VPSError
  = IPRFailed IPRError
  | SherlockFailed SherlockError
  | Couldn'tGetScanId HttpException
  | Couldn'tUpload HttpException
  deriving (Show, Generic)

vpsScan ::
  ( Has ScotlandYard sig m
  , Has (Error VPSError) sig m
  , Has Trace sig m
  , MonadIO m
  ) => Path Abs Dir -> ScanCmdOpts -> m ()
vpsScan basedir ScanCmdOpts{..} = do
  let vpsOpts@VPSOpts{..} = scanVpsOpts
  response <- tagError Couldn'tGetScanId =<< createScotlandYardScan vpsOpts
  let scanId = responseScanId response

  trace $ "Running scan on directory " ++ show basedir
  trace $ "Scan ID from Scotland yard is " ++ show scanId
  trace "[All] Running IPR and Sherlock scans in parallel"
  trace "[Sherlock] Starting Sherlock scan"
  case vpsIpr of
    Just _ -> trace "[IPR] Starting IPR scan"
    Nothing -> trace "[IPR] IPR scan disabled"

  (iprResult, sherlockResult) <- liftIO $ concurrently
                (runError @VPSError $ runIPR $ runScotlandYard $ runTrace $ runIPRScan basedir scanId vpsOpts)
                (runError @VPSError $ runSherlock $ runTrace $ runSherlockScan basedir scanId vpsOpts)
  case (iprResult, sherlockResult) of
    (Right _, Right _) -> trace "[All] Scans complete"
    (Left err, _) -> throwError err
    (_, Left err) -> throwError err

runSherlockScan ::
  ( Has Sherlock sig m
  , Has (Error VPSError) sig m
  , Has Trace sig m
  ) => Path Abs Dir -> Text -> VPSOpts -> m ()
runSherlockScan basedir scanId vpsOpts = do
  tagError SherlockFailed =<< execSherlock basedir scanId vpsOpts
  trace "[Sherlock] Sherlock scan complete"

runIPRScan ::
  ( Has ScotlandYard sig m
  , Has IPR sig m
  , Has (Error VPSError) sig m
  , Has Trace sig m
  ) => Path Abs Dir ->  Text -> VPSOpts -> m ()
runIPRScan basedir scanId vpsOpts@VPSOpts{..} =
  case vpsIpr of
    Just iprOpts -> do
      iprResult <- tagError IPRFailed =<< execIPR basedir iprOpts
      trace "[IPR] IPR scan completed. Posting results to Scotland Yard"

      tagError Couldn'tUpload =<< uploadIPRResults vpsOpts scanId iprResult
      trace "[IPR] Post to Scotland Yard complete"
      trace "[IPR] IPR scan complete"
    Nothing ->
      trace "[IPR] IPR Scan disabled"


validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute
