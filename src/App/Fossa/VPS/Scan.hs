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
import App.Types (BaseDir (..), OverrideProject (..), ProjectRevision (..), ProjectMetadata (..))
import Data.Aeson
import Data.Flag (Flag, fromFlag)
import Data.Text (Text)
import Effect.Logger
import Path
import Fossa.API.Types (ApiOpts(..))

-- | SkipIPRScan bool flag
data SkipIPRScan = SkipIPRScan

scanMain :: BaseDir -> ApiOpts -> ProjectMetadata -> Severity -> OverrideProject -> FilterExpressions -> Flag SkipIPRScan ->  IO ()
scanMain basedir apiOpts metadata logSeverity overrideProject fileFilters skipIprScan = do
  result <- runDiagnostics $ withEmbeddedBinaries $ vpsScan basedir logSeverity overrideProject skipIprScan fileFilters apiOpts metadata
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()

----- main logic

vpsScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) => BaseDir -> Severity -> OverrideProject -> Flag SkipIPRScan -> FilterExpressions -> ApiOpts -> ProjectMetadata -> BinaryPaths -> m ()
vpsScan (BaseDir basedir) logSeverity overrideProject skipIprFlag fileFilters apiOpts metadata binaryPaths = withLogQueue logSeverity $ \queue -> runLogger queue $ do
  -- Build the revision
  ProjectRevision {..} <- mergeOverride overrideProject <$> inferProject basedir

  -- Get Sherlock info
  logDebug "[Sherlock] Retrieving Sherlock information from FOSSA"
  SherlockInfo{..} <- getSherlockInfo apiOpts

  -- Build locator info
  let locator = createLocator projectName sherlockOrgId
  let revisionLocator = createRevisionLocator projectName sherlockOrgId projectRevision

  -- FIXME: use better type here
  let vpsOpts = VPSOpts projectName (Just projectRevision) (fromFlag SkipIPRScan skipIprFlag) fileFilters

  -- Update vpsOpts with overriding scan filter blob. 
  -- Previous uses of `vpsOpts` do not deconstruct the object due to this not yet being overridden.
  (vpsOpts'@VPSOpts{..}, areFiltersOverridden) <- overrideScanFilters apiOpts vpsOpts locator
  logDebug $ pretty $ "[All] Creating project with locator '" <> unRevisionLocator revisionLocator <> "'"

  -- Create scan in Core
  logDebug "[All] Creating project in FOSSA"
  _ <- context "creating project in FOSSA" $ createCoreProject vpsProjectName projectRevision metadata apiOpts

  -- Create scan in SY
  logDebug "[All] Creating scan in Scotland Yard"
  let syOpts = ScotlandYardOpts locator projectRevision sherlockOrgId vpsOpts'
  response <- context "creating scan ID" $ createScotlandYardScan apiOpts syOpts
  let scanId = createScanResponseId response

  -- Run IPR and Sherlock CLIs concurrently
  logDebug . pretty $ "[All] Running scan on directory " ++ show basedir
  logDebug . pretty $ "[All] Scan ID is " <> scanId
  logDebug "[All] Running IPR and Sherlock scans in parallel"
  logDebug "[Sherlock] Starting Sherlock scan"

  let sherlockOpts = SherlockOpts basedir scanId sherlockClientToken sherlockClientId sherlockUrl sherlockOrgId locator projectRevision vpsOpts'
  let runIt = runLogger queue . runDiagnostics . runExecIO
  (iprResult, sherlockResult) <- sendIO $ concurrently
                (runIt $ runIPRScan basedir apiOpts scanId binaryPaths syOpts vpsOpts')
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
  _ <- context "completing project in FOSSA" $ completeCoreProject revisionLocator apiOpts
  _ <- context "updating scan file filter" $ updateScanFileFilter areFiltersOverridden locator fileFilter apiOpts
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
  ) => Path Abs Dir -> ApiOpts -> Text -> BinaryPaths -> ScotlandYardOpts -> VPSOpts -> m ()
runIPRScan basedir apiOpts scanId binaryPaths syOpts vpsOpts =
  if skipIprScan vpsOpts then do
    logDebug "[IPR] IPR scan disabled. Uploading an empty IPR set"
    context "uploading empty scan results" $ uploadIPRResults apiOpts scanId (object ["Files" .= ()]) syOpts
    logDebug "[IPR] Post to Scotland Yard complete"
  else do
    iprResult <- execIPR binaryPaths $ IPROpts basedir vpsOpts
    logDebug "[IPR] IPR scan completed. Posting results to Scotland Yard"
    context "uploading scan results" $ uploadIPRResults apiOpts scanId iprResult syOpts
    logDebug ""
    logDebug "[IPR] Post to Scotland Yard complete"
    logDebug "[IPR] IPR scan complete"

updateScanFileFilter :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m) => Bool -> Locator -> FilterExpressions -> ApiOpts -> m ()
updateScanFileFilter False locator filterBlob fossa = storeUpdatedScanFilters locator filterBlob fossa
updateScanFileFilter True _ _ _ = do
  logDebug "[All] Scan file filter was set by FOSSA server, skipping update"
  pure ()
