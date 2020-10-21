{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan
  ( scanMain,
    SkipIPRScan (..)
  ) where

import Control.Effect.Lift (Lift, sendIO)
import Control.Carrier.Diagnostics
import Effect.Exec
import System.Exit (exitFailure)
import Control.Concurrent.Async (concurrently)

import App.Fossa.VPS.EmbeddedBinary
import App.Fossa.VPS.Scan.Core
import App.Fossa.VPS.Scan.RunIPR
import App.Fossa.VPS.Scan.RunSherlock
import App.Fossa.VPS.Scan.ScotlandYard
import App.Fossa.VPS.Types
import App.Fossa.ProjectInference
import App.Types (BaseDir (..), ApiKey (..), OverrideProject (..), ProjectRevision (..))
import Data.Aeson
import Data.Text (Text)
import Effect.Logger
import Path
import Text.URI (URI)

newtype SkipIPRScan = SkipIPRScan {unSkipIPRScan :: Bool}

scanMain :: URI -> BaseDir -> ApiKey -> Severity -> OverrideProject -> FilterExpressions -> SkipIPRScan ->  IO ()
scanMain baseuri basedir apikey logSeverity overrideProject fileFilters skipIprScan = do
  let fossaOpts = FossaOpts baseuri $ unApiKey apikey
      partVpsOpts = PartialVPSOpts fossaOpts (unSkipIPRScan skipIprScan) fileFilters

  result <- runDiagnostics $ withEmbeddedBinaries $ vpsScan basedir logSeverity overrideProject partVpsOpts
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()

----- main logic

vpsScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) => BaseDir -> Severity -> OverrideProject -> PartialVPSOpts -> BinaryPaths -> m ()
vpsScan (BaseDir basedir) logSeverity overrideProject partVpsOpts binaryPaths = withLogQueue logSeverity $ \queue -> runLogger queue $ do
  -- Build the revision
  ProjectRevision {..} <- mergeOverride overrideProject <$> inferProject basedir

  -- Get Sherlock info
  logDebug "[Sherlock] Retrieving Sherlock information from FOSSA"
  SherlockInfo{..} <- getSherlockInfo (fossaOpts partVpsOpts)

  -- Build locator info
  let locator = createLocator projectName sherlockOrgId
  let revisionLocator = createRevisionLocator projectName sherlockOrgId projectRevision

  -- FIXME: use better type here
  let PartialVPSOpts {..} = partVpsOpts
  let vpsOpts = VPSOpts fossaOpts projectName (Just projectRevision) partSkipIprScan partFileFilter

  -- Update vpsOpts with overriding scan filter blob. 
  -- Previous uses of `vpsOpts` do not deconstruct the object due to this not yet being overridden.
  (vpsOpts'@VPSOpts{..}, areFiltersOverridden) <- overrideScanFilters vpsOpts locator
  logDebug $ pretty $ "[All] Creating project with locator '" <> unRevisionLocator revisionLocator <> "'"

  -- Create scan in Core
  logDebug "[All] Creating project in FOSSA"
  _ <- context "creating project in FOSSA" $ createCoreProject vpsProjectName projectRevision fossa

  -- Create scan in SY
  logDebug "[All] Creating scan in Scotland Yard"
  let syOpts = ScotlandYardOpts locator projectRevision sherlockOrgId vpsOpts'
  response <- context "creating scan ID" $ createScotlandYardScan syOpts
  let scanId = responseScanId response

  -- Run IPR and Sherlock CLIs concurrently
  logDebug . pretty $ "[All] Running scan on directory " ++ show basedir
  logDebug . pretty $ "[All] Scan ID is " <> scanId
  logDebug "[All] Running IPR and Sherlock scans in parallel"
  logDebug "[Sherlock] Starting Sherlock scan"

  let sherlockOpts = SherlockOpts basedir scanId sherlockClientToken sherlockClientId sherlockUrl sherlockOrgId locator projectRevision vpsOpts'
  let runIt = runLogger queue . runDiagnostics . runExecIO
  (iprResult, sherlockResult) <- sendIO $ concurrently
                (runIt $ runIPRScan basedir scanId binaryPaths syOpts vpsOpts')
                (runIt $ runSherlockScan binaryPaths sherlockOpts)

  case (iprResult, sherlockResult) of
    (Right _, Right _) -> logDebug "[All] Scans complete"
    (Left iprFailure, _) -> do
      logDebug "[IPR] Failed to scan"
      logDebug $ renderFailureBundle iprFailure
      sendIO exitFailure
    (_, Left sherlockFailure) -> do
      logDebug "[Sherlock] Failed to scan"
      logDebug $ renderFailureBundle sherlockFailure
      sendIO exitFailure

  logDebug "[All] Completing scan in FOSSA"
  _ <- context "completing project in FOSSA" $ completeCoreProject revisionLocator fossa
  _ <- context "updating scan file filter" $ updateScanFileFilter areFiltersOverridden locator fileFilter fossa
  logDebug "[All] Project is ready to view in FOSSA (Sherlock forensics may still be pending)"

runSherlockScan ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) => BinaryPaths -> SherlockOpts -> m ()
runSherlockScan binaryPaths sherlockOpts = do
  execSherlock binaryPaths sherlockOpts
  logDebug "[Sherlock] Sherlock scan complete"

runIPRScan ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) => Path Abs Dir -> Text -> BinaryPaths -> ScotlandYardOpts -> VPSOpts -> m ()
runIPRScan basedir scanId binaryPaths syOpts vpsOpts =
  if skipIprScan vpsOpts then do
    logDebug "[IPR] IPR scan disabled. Uploading an empty IPR set"
    context "uploading empty scan results" $ uploadIPRResults scanId (object ["Files" .= ()]) syOpts
    logDebug "[IPR] Post to Scotland Yard complete"
  else do
    iprResult <- execIPR binaryPaths $ IPROpts basedir vpsOpts
    logDebug "[IPR] IPR scan completed. Posting results to Scotland Yard"
    context "uploading scan results" $ uploadIPRResults scanId iprResult syOpts
    logDebug ""
    logDebug "[IPR] Post to Scotland Yard complete"
    logDebug "[IPR] IPR scan complete"

updateScanFileFilter :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m) => Bool -> Locator -> FilterExpressions -> FossaOpts -> m ()
updateScanFileFilter False locator filterBlob fossa = storeUpdatedScanFilters locator filterBlob fossa
updateScanFileFilter True _ _ _ = do
  logDebug "[All] Scan file filter was set by FOSSA server, skipping update"
  pure ()