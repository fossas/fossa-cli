module App.Fossa.VSI.Analyze (
  runVsiAnalysis,
) where

import App.Fossa.FossaAPIV1 (vsiAddFilesToScan, vsiCompleteScan, vsiCreateScan, vsiDownloadInferences, vsiScanAnalysisStatus)
import App.Fossa.VSI.Fingerprint (Combined, fingerprint)
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import App.Types (ProjectRevision)
import Control.Algebra (Has)
import Control.Concurrent (threadDelay)
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, fromEither)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.StickyLogger (StickyLogger, logSticky)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.String.Conversion (toText)
import Discovery.Filters (AllFilters, combinedPaths, excludeFilters, includeFilters)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipAll), walk')
import Effect.Logger (Logger, logDebug, pretty)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Path (Abs, Dir, File, Path, Rel, SomeBase (Abs, Rel), isProperPrefixOf, (</>))
import Path.Extra (isChildOf, tryMakeRelative)

runVsiAnalysis ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  ApiOpts ->
  ProjectRevision ->
  AllFilters ->
  m ([VSI.Locator], [IAT.UserDep])
runVsiAnalysis dir apiOpts projectRevision filters = context "VSI" $ do
  -- TODO(kit): Figure out how to parallelize fingerprinting
  -- TODO(kit): Figure out how to walk expanded files
  -- TODO(kit): Figure out how to filter walked files

  scanID <- context "Create scan in backend" $ vsiCreateScan apiOpts projectRevision
  logDebug . pretty $ "Created Scan ID: " <> toText scanID

  -- TODO(kit): Make both walking and uploading streaming so we work on 1000 fingerprint chunks at a time.
  fingerprints <- context "Fingerprint files" $ runFingerprint dir (toPathFilters dir filters)
  context "Upload fingerprints" $ vsiAddFilesToScan apiOpts scanID fingerprints

  context "Finalize scan files" $ vsiCompleteScan apiOpts scanID
  context "Waiting for backend analysis to complete" $ waitForAnalysis apiOpts scanID

  discoveredRawLocators <- context "Download analysis results" $ vsiDownloadInferences apiOpts scanID
  parsedLocators <- context "Parse analysis results" . fromEither $ traverse VSI.parseLocator discoveredRawLocators

  let userDefinedDeps = map IAT.toUserDep $ filter VSI.isUserDefined parsedLocators
  let allOtherDeps = filter (not . VSI.isUserDefined) parsedLocators
  pure (allOtherDeps, userDefinedDeps)

-- | Walk the directory tree starting from the root directory, fingerprinting any files that are children of the root directory.
runFingerprint :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> PathFilters -> m (Map (Path Rel File) Combined)
runFingerprint root filters = flip walk' root $ \dir _ files ->
  if shouldFingerprint dir
    then do
      fingerprints <- traverse (fingerprintRelativeFiles root) files
      pure (Map.fromList $ catMaybes fingerprints, WalkContinue)
    else pure (Map.empty, WalkSkipAll)
  where
    shouldFingerprint dir = filters `allow` dir && dir `isChildOf` root
    fingerprintRelativeFiles dir file = case tryMakeRelative dir file of
      Abs _ -> pure Nothing
      Rel rel -> do
        fp <- fingerprint file
        pure $ Just (rel, fp)

-- | Wait for analysis to complete
waitForAnalysis ::
  (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has StickyLogger sig m) =>
  ApiOpts ->
  VSI.ScanID ->
  m ()
waitForAnalysis apiOpts scanID = do
  status <- vsiScanAnalysisStatus apiOpts scanID
  case status of
    VSI.AnalysisFailed -> fatalText "Backend analysis failed. If this persists, please contact FOSSA and provide debug logs (generated with --debug)"
    VSI.AnalysisFinished -> do
      logDebug "Backend analysis complete"
      pure ()
    VSI.AnalysisPending -> do
      logSticky "Backend analysis is enqueued, waiting to start..."
      waitForAnalysis apiOpts scanID
    VSI.AnalysisInformational msg -> do
      logDebug . pretty $ "Backend analysis status: '" <> msg <> "'"
      logSticky "Backend analysis running"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForAnalysis apiOpts scanID
  where
    pollDelaySeconds = 8

-- | PathFilters is a specialized filter mechanism that operates only on absolute directory paths.
data PathFilters = PathFilters
  { include :: [Path Abs Dir]
  , exclude :: [Path Abs Dir]
  }
  deriving (Show)

toPathFilters :: Path Abs Dir -> AllFilters -> PathFilters
toPathFilters root filters =
  PathFilters
    { include = map (root </>) (combinedPaths $ includeFilters filters)
    , exclude = map (root </>) (combinedPaths $ excludeFilters filters)
    }

allow :: PathFilters -> Path Abs Dir -> Bool
allow filters dir = (not shouldExclude) && shouldInclude
  where
    shouldExclude = (isPrefixedOrEqual dir) `any` (exclude filters)
    shouldInclude = null (include filters) || (isPrefixedOrEqual dir) `any` (include filters)
    isPrefixedOrEqual a b = a == b || isProperPrefixOf b a -- swap order of isProperPrefixOf comparison because we want to know if dir is prefixed by any filter
