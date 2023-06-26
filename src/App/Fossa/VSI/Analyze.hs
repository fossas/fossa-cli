{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VSI.Analyze (
  runVsiAnalysis,
) where

import App.Fossa.VSI.Fingerprint (Combined, fingerprint)
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types (ScanID (..))
import App.Fossa.VSI.Types qualified as VSI
import App.Types (ProjectRevision)
import App.Util (ancestryDerived, ancestryDirect, FileAncestry (..))
import Control.Algebra (Has)
import Control.Carrier.AtomicCounter (runAtomicCounter)
import Control.Carrier.Diagnostics (runDiagnosticsIO, withResult)
import Control.Carrier.StickyLogger (runStickyLogger)
import Control.Carrier.TaskPool (Progress (..), withTaskPool)
import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (TBMChan, closeTBMChan, newTBMChanIO, readTBMChan, writeTBMChan)
import Control.Effect.Diagnostics (Diagnostics, context, fatalOnSomeException, fatalText, fromEither, recover)
import Control.Effect.Finally (Finally)
import Control.Effect.FossaApiClient (FossaApiClient, addFilesToVsiScan, completeVsiScan, createVsiScan, getVsiInferences, getVsiScanAnalysisStatus)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Stack (Stack)
import Control.Effect.StickyLogger (StickyLogger, logSticky, logSticky')
import Control.Effect.TaskPool (TaskPool, forkTask)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Archive (withArchive')
import Discovery.Filters (AllFilters, combinedPaths, excludeFilters, includeFilters)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipAll), walk)
import Effect.Logger (Color (..), Logger, Severity (SevError, SevInfo, SevWarn), annotate, color, hsep, logDebug, logInfo, plural, pretty)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, File, Path, Rel, isProperPrefixOf, (</>))
import Path qualified as P

runVsiAnalysis ::
  ( Has (Lift IO) sig m
  , Has Finally sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  ProjectRevision ->
  AllFilters ->
  m ([VSI.Locator], [IAT.UserDep])
runVsiAnalysis dir projectRevision filters = context "VSI" $ do
  -- If we try to run with fewer than 2 capabilities, STM will deadlock
  capabilities <- max 2 <$> sendIO getNumCapabilities
  logDebug $ hsep ["Running with capabilites:", pretty capabilities]

  scanID <- context "Create scan in backend" $ createVsiScan projectRevision
  logInfo . pretty $ "Created Scan ID: " <> unScanID scanID

  -- Allow up to 2x the buffer size to be stored:
  -- - runBatchUploader buffers up to uploadBufferSize
  -- - newTBMChanIO also buffers up to uploadBufferSize
  files <- sendIO $ newTBMChanIO uploadBufferSize
  context "Fingerprint files"
    . withTaskPool capabilities (updateProgress "Fingerprint and upload")
    . runAtomicCounter
    $ do
      context "Discover fingerprints" . forkTask $ runFingerprintDiscovery capabilities files dir filters
      context "Upload fingerprints" . forkTask $ runBatchUploader files scanID

  logInfo "Finalizing scan"
  context "Finalize scan" $ completeVsiScan scanID

  logInfo "Waiting for cloud analysis"
  context "Wait for cloud analysis" $ waitForAnalysis scanID

  discoveredRawLocators <- context "Download analysis results" $ getVsiInferences scanID
  when (null discoveredRawLocators) $ fatalText "No dependencies discovered with VSI"

  parsedLocators <- context "Parse analysis results" . fromEither $ traverse VSI.parseLocator discoveredRawLocators

  let userDefinedDeps = map IAT.toUserDep $ filter VSI.isUserDefined parsedLocators
  let allOtherDeps = filter (not . VSI.isUserDefined) parsedLocators
  pure (allOtherDeps, userDefinedDeps)

uploadBufferSize :: Int
uploadBufferSize = 1000

runFingerprintDiscovery ::
  ( Has (Lift IO) sig m
  , Has Stack sig m
  , Has Finally sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  -- | Parallelization factor
  Int ->
  -- | Channel on which new files are collected
  TBMChan (Path Rel File, Combined) ->
  Path Abs Dir ->
  AllFilters ->
  m ()
runFingerprintDiscovery capabilities files dir filters = do
  runStickyLogger SevInfo . withTaskPool capabilities (updateProgress " > Fingerprint files") . runAtomicCounter $ do
    let pathFilters = withDefaultFilters dir $ toPathFilters dir filters
    res <- runDiagnosticsIO $ discover files pathFilters dir ancestryDirect
    withResult SevError SevWarn res (const (pure ()))

  logDebug "Finished processing files"
  sendIO . atomically $ closeTBMChan files

-- | Collects fingerprinted files coming in over a channel and uploads them to the server in chunks.
runBatchUploader ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  -- | Channel on which new files are collected
  TBMChan (Path Rel File, Combined) ->
  -- | API options used for the upload
  ScanID ->
  m ()
runBatchUploader files scanID = runStickyLogger SevInfo $ do
  logSticky " > Buffering first chunk..."
  collect (0 :: Int) []
  where
    await = sendIO . atomically $ readTBMChan files
    upload [] = pure ()
    upload buf = do
      logDebug . pretty $ "Upload chunk of " <> show (length buf) <> " file(s)"
      addFilesToVsiScan scanID $ Map.fromList buf
    collect uploadCount buf | length buf >= uploadBufferSize = do
      logDebug "Upload buffer is full, uploading chunk"
      upload buf
      let newCount = uploadCount + 1
      logSticky $ " > Uploaded " <> toText (show newCount) <> " chunk" <> plural "" "s" newCount
      collect newCount []
    collect uploadCount buf = do
      next <- await
      case next of
        Nothing -> do
          logDebug "No more files, uploading final chunk"
          upload buf
        Just f -> do
          logDebug . pretty $ "Appending " <> toText (fst f) <> " to upload buffer (" <> (toText . show) (length buf + 1) <> " / 1000)"
          collect uploadCount $ f : buf

-- | Walk the directory tree starting from the root directory, fingerprinting any files that are children of the root directory.
--
-- Results in a map of file paths to fingerprint values. The file paths are relative *to the directory being scanned*.
-- This means that the following directory structure:
--
-- > ~/
-- >   my_project/
-- >     main.c
-- >     main.h
-- >     lib/
-- >       lib.c
-- >       lib.h
--
-- Is reported as:
--
-- > Map.fromList
-- >   [ ("main.c", { <fingerprints> })
-- >   , ("main.h", { <fingerprints> })
-- >   , ("lib/lib.c", { <fingerprints> })
-- >   , ("lib/lib.h", { <fingerprints> })
-- >   ]
--
-- Also recursively unpacks archives and fingerprints their contents.
--
-- Files extracted from archives are *actually* extracted to a temporary directory,
-- but are reported as though they come from a directory with the same path as the archive
-- with the literal `!_fossa.virtual_!` appended.
--
-- The archive itself is also still fingerprinted and reported.
--
-- Specifically, given the following directory structure:
--
-- > ~/
-- >  my_project/
-- >    lib.zip
-- >      other.tar
-- >        README.md
--
-- The directory structure expands to the following "virtual" directory structure:
--
-- > ~/
-- >  my_project/
-- >    lib.zip  <-- Reported as a fingerprint
-- >    lib.zip!_fossa.virtual_!/
-- >      other.tar  <-- Reported as a fingerprint
-- >      other.tar!_fossa.virtual_!/
-- >        README.md  <-- Reported as a fingerprint
--
-- This means that the following directory structure:
--
-- > ~/
-- >   my_project/
-- >     main.c
-- >     main.h
-- >     lib/
-- >       lib.c
-- >       lib.h
-- >     external/
-- >       ffmpeg.zip
-- >         ffmpeg.c
-- >         ffmpeg.h
--
-- Is reported as:
--
-- > Map.fromList
-- >   [ ("main.c", { <fingerprints> })
-- >   , ("main.h", { <fingerprints> })
-- >   , ("lib/lib.c", { <fingerprints> })
-- >   , ("lib/lib.h", { <fingerprints> })
-- >   , ("external/ffmpeg.zip", { <fingerprints> })
-- >   , ("external/ffmpeg.zip!_fossa.virtual_!/ffmpeg.c", { <fingerprints> })
-- >   , ("external/ffmpeg.zip!_fossa.virtual_!/ffmpeg.h", { <fingerprints> })
-- >   ]
--
-- The `!_fossa.virtual_!` suffix is a server-side invariant.
-- Similarly, it is a server-side invariant that the `!_fossa.virtual_!`-suffixed directory is a sibling of the original archive.
discover ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Finally sig m
  , Has TaskPool sig m
  , Has Logger sig m
  ) =>
  TBMChan (Path Rel File, Combined) ->
  -- | Filters used to exclude scanned directories
  PathFilters ->
  -- | Root to discover at
  Path Abs Dir ->
  -- | Path rendering
  (Path Abs Dir -> Path Abs File -> m (Path Rel File)) ->
  m ()
discover output filters root renderAncestry =
  context "discover" $ do
    logDebug . pretty $ "walking new root: " <> toText root
    flip walk root $ \dir _ files -> handle dir files
  where
    handle dir files | filters `allow` dir = do
      logDebug . pretty $ "processing dir: " <> toText dir
      traverse_ (forkTask . process) files
      pure WalkContinue
    handle dir _ = do
      logDebug . pretty $ "skip dir: " <> toText dir
      pure WalkSkipAll
    process file = context "process file" $ do
      -- Synchronously fingerprint the file and compute its logical path.
      fp <- fingerprint file
      logicalPath <- renderAncestry root file

      -- Fork an async task to walk the contents of the archive, if the file is an archive.
      -- Note: if you're ever looking at the @TaskPool@ progress message and are like "why does it show 2x the files as are actually in the project?"
      -- This is the reason. We fork a task for every file to process its fingerprint, then fork a second task to (maybe) extract it as an archive.
      forkTask . recover . fatalOnSomeException "extract archive" . withArchive' file $ \archiveRoot -> context "walking into child archive" $ do
        logDebug . pretty $ "walking into " <> toText file <> " as archive"
        logicalParent <- convertArchiveSuffix logicalPath
        discover output filters archiveRoot $ ancestryDerived $ FileAncestry logicalParent

      -- Report the fingerprint and logical path for computing this chunk.
      logDebug . pretty $ "report logical path: " <> toText logicalPath
      sendIO . atomically $ writeTBMChan output (logicalPath, fp)

-- | Converts a relative file path into a relative directory, where the passed in file path is suffixed by the archive suffix literal.
-- In other words, this:
--
-- > "external/lib.zip" :: Path Rel File
--
-- Becomes this:
--
-- > "external/lib.zip!_fossa.virtual_!/" :: Path Rel Dir
convertArchiveSuffix :: (Has Diagnostics sig m) => Path Rel File -> m (Path Rel Dir)
convertArchiveSuffix file = do
  name <- fromEither . P.parseRelDir $ P.toFilePath (P.filename file) <> "!_fossa.virtual_!"
  pure $ P.parent file </> name

-- | Wait for analysis to complete
waitForAnalysis ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  ) =>
  VSI.ScanID ->
  m ()
waitForAnalysis scanID = do
  status <- getVsiScanAnalysisStatus scanID
  case status of
    VSI.AnalysisFailed ->
      fatalText "Backend analysis failed. If this persists, please contact FOSSA and provide debug logs (generated with --debug)"
    VSI.AnalysisFinished ->
      logDebug "Backend analysis complete"
    VSI.AnalysisPending -> do
      logSticky "Backend analysis is enqueued, waiting to start..."
      waitForAnalysis scanID
    VSI.AnalysisInformational msg -> do
      logDebug . pretty $ "Backend analysis status: '" <> msg <> "'"
      logSticky "Backend analysis running"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForAnalysis scanID
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

withDefaultFilters :: Path Abs Dir -> PathFilters -> PathFilters
withDefaultFilters root filters =
  PathFilters
    { include = include filters
    , exclude = dotgit : exclude filters
    }
  where
    dotgit = root </> $(P.mkRelDir ".git")

allow :: PathFilters -> Path Abs Dir -> Bool
allow filters dir = (not shouldExclude) && shouldInclude
  where
    shouldExclude = (isPrefixedOrEqual dir) `any` (exclude filters)
    shouldInclude = null (include filters) || (isPrefixedOrEqual dir) `any` (include filters)
    isPrefixedOrEqual a b = a == b || isProperPrefixOf b a -- swap order of isProperPrefixOf comparison because we want to know if dir is prefixed by any filter

updateProgress :: Has StickyLogger sig m => Text -> Progress -> m ()
updateProgress status Progress{..} =
  logSticky'
    ( renderStatus
        <> "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )
  where
    renderStatus = pretty $ if Text.null status then "" else status <> ": "
