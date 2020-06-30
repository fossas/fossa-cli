module App.VPSScan.Scan
  ( scanMain
  , ScanCmdOpts(..)
  , VPSOpts(..)
  ) where

import Prologue

import Control.Carrier.Diagnostics
import Effect.Exec
import System.Exit (exitFailure)
import Control.Concurrent.Async (concurrently)
import Control.Carrier.Trace.Printing

import App.VPSScan.Types
import App.VPSScan.Scan.RunSherlock
import App.VPSScan.Scan.ScotlandYard
import App.VPSScan.Scan.RunIPR
import App.Util (validateDir)

data ScanCmdOpts = ScanCmdOpts
  { cmdBasedir :: FilePath
  , scanVpsOpts :: VPSOpts
  } deriving Generic

scanMain :: ScanCmdOpts -> IO ()
scanMain opts@ScanCmdOpts{..} = do
  basedir <- validateDir cmdBasedir
  result <- runDiagnostics $ runTrace $ vpsScan basedir opts
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()

----- main logic

vpsScan ::
  ( Has Diagnostics sig m
  , Has Trace sig m
  , MonadIO m
  ) => Path Abs Dir -> ScanCmdOpts -> m ()
vpsScan basedir ScanCmdOpts{..} = do
  let vpsOpts@VPSOpts{..} = scanVpsOpts
  response <- context "creating scan ID" $ createScotlandYardScan vpsOpts
  let scanId = responseScanId response

  trace $ "Running scan on directory " ++ show basedir
  trace $ "Scan ID from Scotland yard is " ++ show scanId
  trace "[All] Running IPR and Sherlock scans in parallel"
  trace "[Sherlock] Starting Sherlock scan"
  case vpsIpr of
    Just _ -> trace "[IPR] Starting IPR scan"
    Nothing -> trace "[IPR] IPR scan disabled"

  let runIt = runDiagnostics . runExecIO . runTrace
  (iprResult, sherlockResult) <- liftIO $ concurrently
                (runIt $ runIPRScan basedir scanId vpsOpts)
                (runIt $ runSherlockScan basedir scanId vpsOpts)
  case (iprResult, sherlockResult) of
    (Right _, Right _) -> trace "[All] Scans complete"
    (Left iprFailure, _) -> do
      trace "[IPR] Failed to scan"
      trace (show $ renderFailureBundle iprFailure)
      liftIO exitFailure
    (_, Left sherlockFailure) -> do
      trace "[Sherlock] Failed to scan"
      trace (show $ renderFailureBundle sherlockFailure)
      liftIO exitFailure

runSherlockScan ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has Trace sig m
  ) => Path Abs Dir -> Text -> VPSOpts -> m ()
runSherlockScan basedir scanId vpsOpts = do
  execSherlock basedir scanId vpsOpts
  trace "[Sherlock] Sherlock scan complete"

runIPRScan ::
  ( Has Diagnostics sig m
  , Has Trace sig m
  , Has Exec sig m
  , MonadIO m
  ) => Path Abs Dir ->  Text -> VPSOpts -> m ()
runIPRScan basedir scanId vpsOpts@VPSOpts{..} =
  case vpsIpr of
    Just iprOpts -> do
      iprResult <- execIPR basedir iprOpts
      trace "[IPR] IPR scan completed. Posting results to Scotland Yard"

      context "uploading scan results" $ uploadIPRResults vpsOpts scanId iprResult
      trace "[IPR] Post to Scotland Yard complete"
      trace "[IPR] IPR scan complete"
    Nothing ->
      trace "[IPR] IPR Scan disabled"