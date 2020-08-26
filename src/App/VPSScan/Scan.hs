{-# LANGUAGE RecordWildCards #-}

module App.VPSScan.Scan
  ( scanMain
  , ScanCmdOpts(..)
  , VPSOpts(..)
  ) where

import Control.Effect.Lift (Lift, sendIO)
import Control.Carrier.Diagnostics
import Effect.Exec
import System.Exit (exitFailure)
import Control.Concurrent.Async (concurrently)
import Control.Carrier.Trace.Printing

import App.VPSScan.Types
import App.VPSScan.Scan.ScotlandYard
import App.VPSScan.Scan.RunSherlock
import App.VPSScan.Scan.RunIPR
import App.VPSScan.Scan.Core
import App.VPSScan.EmbeddedBinary
import App.Types (BaseDir (..))
import App.Util (validateDir)
import Data.Text (unpack)
import Control.Effect.Exception (bracket)
import Path
import Data.Text (Text)

data ScanCmdOpts = ScanCmdOpts
  { cmdBasedir :: FilePath
  , scanVpsOpts :: VPSOpts
  }

scanMain :: ScanCmdOpts -> IO ()
scanMain opts@ScanCmdOpts{..} = do
  basedir <- validateDir cmdBasedir
  result <- runDiagnostics $ runTrace $ bracket extractEmbeddedBinaries cleanupExtractedBinaries $ vpsScan (unBaseDir basedir) opts
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()

----- main logic

vpsScan ::
  ( Has Diagnostics sig m
  , Has Trace sig m
  , Has (Lift IO) sig m
  ) => Path Abs Dir -> ScanCmdOpts -> BinaryPaths -> m ()
vpsScan basedir ScanCmdOpts{..} binaryPaths = do
  let vpsOpts@VPSOpts{..} = scanVpsOpts
  
  -- Build the revision
  projectRevision <- buildRevision userProvidedRevision

  -- Get Sherlock info
  trace "[Sherlock] Retrieving Sherlock information from FOSSA"
  SherlockInfo{..} <- getSherlockInfo fossa
  let locator = createLocator projectName sherlockOrgId
  let revisionLocator = createRevisionLocator projectName sherlockOrgId projectRevision
  trace $ unpack $ "[All] Creating project with locator '" <> unLocator revisionLocator <> "'"

  -- Create scan in Core
  trace "[All] Creating project in FOSSA"
  _ <- context "creating project in FOSSA" $ createCoreProject projectName projectRevision fossa

  -- Create scan in SY
  trace $ "[All] Creating scan in Scotland Yard"
  let syOpts = ScotlandYardOpts locator projectRevision sherlockOrgId vpsOpts
  response <- context "creating scan ID" $ createScotlandYardScan syOpts
  let scanId = responseScanId response

  -- Run IPR and Sherlock CLIs concurrently
  trace $ "[All] Running scan on directory " ++ show basedir
  trace $ unpack $ "[All] Scan ID is " <> scanId
  trace "[All] Running IPR and Sherlock scans in parallel"
  trace "[Sherlock] Starting Sherlock scan"

  let sherlockOpts = SherlockOpts basedir scanId sherlockClientToken sherlockClientId sherlockUrl sherlockOrgId locator projectRevision vpsOpts
  let runIt = runDiagnostics . runExecIO . runTrace
  (iprResult, sherlockResult) <- sendIO $ concurrently
                (runIt $ runIPRScan basedir scanId binaryPaths syOpts vpsOpts)
                (runIt $ runSherlockScan binaryPaths sherlockOpts)

  case (iprResult, sherlockResult) of
    (Right _, Right _) -> trace "[All] Scans complete"
    (Left iprFailure, _) -> do
      trace "[IPR] Failed to scan"
      trace (show $ renderFailureBundle iprFailure)
      sendIO exitFailure
    (_, Left sherlockFailure) -> do
      trace "[Sherlock] Failed to scan"
      trace (show $ renderFailureBundle sherlockFailure)
      sendIO exitFailure

  trace $ "[All] Completing scan in FOSSA"
  _ <- context "completing project in FOSSA" $ completeCoreProject (unLocator revisionLocator) fossa
  trace $ "[All] Project is ready to view in FOSSA (Sherlock forensics may still be pending)"

runSherlockScan ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has Trace sig m
  ) => BinaryPaths -> SherlockOpts -> m ()
runSherlockScan binaryPaths sherlockOpts = do
  execSherlock binaryPaths sherlockOpts
  trace "[Sherlock] Sherlock scan complete"

runIPRScan ::
  ( Has Diagnostics sig m
  , Has Trace sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) => Path Abs Dir -> Text -> BinaryPaths -> ScotlandYardOpts -> VPSOpts -> m ()
runIPRScan basedir scanId binaryPaths syOpts vpsOpts =
  if skipIprScan vpsOpts then
    trace "[IPR] IPR scan disabled"
  else do
    iprResult <- execIPR binaryPaths $ IPROpts basedir vpsOpts
    trace "[IPR] IPR scan completed. Posting results to Scotland Yard"

    context "uploading scan results" $ uploadIPRResults scanId iprResult syOpts
    trace "[IPR] Post to Scotland Yard complete"
    trace "[IPR] IPR scan complete"
