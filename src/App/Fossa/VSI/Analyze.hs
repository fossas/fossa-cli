{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.Analyze (
  runVsiAnalysis,
) where

import App.Fossa.FossaAPIV1 (vsiAddFilesToScan, vsiCompleteScan, vsiCreateScan, vsiDownloadInferences, vsiScanAnalysisStatus)
import App.Fossa.VSI.Fingerprint (Combined, fingerprint)
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types (ScanID (..))
import App.Fossa.VSI.Types qualified as VSI
import App.Types (ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.AtomicCounter (runAtomicCounter)
import Control.Carrier.Diagnostics (runDiagnosticsIO, withResult)
import Control.Carrier.Output.IO (Output, output, runOutput)
import Control.Carrier.TaskPool (Progress (..), withTaskPool)
import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Effect.Diagnostics (Diagnostics, context, fatalOnSomeException, fatalText, fromEither, recover)
import Control.Effect.Finally (Finally)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.StickyLogger (StickyLogger, logSticky, logSticky')
import Control.Effect.TaskPool (TaskPool, forkTask)
import Data.Foldable (traverse_)
import Data.List.Split (chunksOf)
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Archive (withArchive')
import Discovery.Filters (AllFilters, combinedPaths, excludeFilters, includeFilters)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipAll), walk)
import Effect.Logger (Color (..), Logger, Severity (SevError), annotate, color, logDebug, logInfo, pretty)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Path (Abs, Dir, File, Path, Rel, SomeBase (Abs, Rel), isProperPrefixOf, toFilePath, (</>))
import Path qualified as P
import Path.Extra (tryMakeRelative)

runVsiAnalysis ::
  ( Has (Lift IO) sig m
  , Has Finally sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has StickyLogger sig m
  ) =>
  Path Abs Dir ->
  ApiOpts ->
  ProjectRevision ->
  AllFilters ->
  m ([VSI.Locator], [IAT.UserDep])
runVsiAnalysis dir apiOpts projectRevision filters = context "VSI" $ do
  -- TODO(kit): Reuse capabilities from parent
  capabilities <- sendIO getNumCapabilities

  -- TODO(kit): Figure out how to stream fingerprinting
  (files, _) <- context "Fingerprint files"
    . runOutput @(Path Rel File, Combined)
    . withTaskPool capabilities (updateProgress "Fingerprint files")
    . runAtomicCounter
    . forkTask
    $ do
      res <- runDiagnosticsIO $ discover (toPathFilters dir filters) dir ancestryDirect
      withResult SevError res (const (pure ()))
  logDebug . pretty $ "Fingerprinted " <> (toText . show $ length files) <> " files"

  scanID <- context "Create scan in backend" $ vsiCreateScan apiOpts projectRevision
  logInfo . pretty $ "Created Scan ID: " <> unScanID scanID

  logInfo . pretty $ "Uploading " <> toText (show $ length files) <> " fingerprints"
  context "Upload fingerprints"
    -- Running 4 requests in parallel isn't for any real reason, just a generic choice.
    -- The idea is that we don't want to saturate their bandwidth with too many parallel requests.
    . withTaskPool 4 (updateProgress "Upload fingerprint batches")
    . runAtomicCounter
    . forkTask
    $ traverse_ (forkTask . vsiAddFilesToScan apiOpts scanID . Map.fromList) $ chunksOf 1000 files

  logInfo "Finalizing scan"
  context "Finalize scan" $ vsiCompleteScan apiOpts scanID

  logInfo "Waiting for cloud analysis"
  context "Wait for cloud analysis" $ waitForAnalysis apiOpts scanID

  discoveredRawLocators <- context "Download analysis results" $ vsiDownloadInferences apiOpts scanID
  parsedLocators <- context "Parse analysis results" . fromEither $ traverse VSI.parseLocator discoveredRawLocators

  let userDefinedDeps = map IAT.toUserDep $ filter VSI.isUserDefined parsedLocators
  let allOtherDeps = filter (not . VSI.isUserDefined) parsedLocators
  pure (allOtherDeps, userDefinedDeps)

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
  , Has (Output (Path Rel File, Combined)) sig m
  ) =>
  PathFilters ->
  -- | Root to discover at
  Path Abs Dir ->
  -- | Path rendering
  (Path Abs Dir -> Path Abs File -> m (Path Rel File)) ->
  m ()
discover filters root renderAncestry =
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
      forkTask . recover . fatalOnSomeException "extract archive" . withArchive' file $ \archiveRoot -> context "walking into child archive" $ do
        logDebug . pretty $ "walking into " <> toText file <> " as archive"
        logicalParent <- convertArchiveSuffix logicalPath
        discover filters archiveRoot $ ancestryDerived logicalParent

      -- Report the fingerprint and logical path for computing this chunk.
      logDebug . pretty $ "report logical path: " <> toText logicalPath
      output (logicalPath, fp)

-- | Renders the relative path from the provided directory to the file.
-- If the path cannot be made relative, fatally exits through the diagnostic effect.
ancestryDirect :: Has Diagnostics sig m => Path Abs Dir -> Path Abs File -> m (Path Rel File)
ancestryDirect dir file = case tryMakeRelative dir file of
  Abs _ -> fatalText $ "failed to make " <> toText (toFilePath file) <> " relative to " <> toText (toFilePath dir)
  Rel rel -> pure rel

-- | Renders the relative path from the provided directory to the file, prepended with the provided relative directory as a parent.
-- If the path cannot be made relative, fatally exits through the diagnostic effect.
ancestryDerived :: Has Diagnostics sig m => Path Rel Dir -> Path Abs Dir -> Path Abs File -> m (Path Rel File)
ancestryDerived parent dir file = do
  rel <- ancestryDirect dir file
  pure $ parent </> rel

-- | Converts a relative file path into a relative directory, where the passed in file path is suffixed by the archive suffix literal.
-- In other words, this:
--
-- > "external/lib.zip" :: Path Rel File
--
-- Becomes this:
--
-- > "external/lib.zip!_fossa.virtual_!/" :: Path Rel Dir
convertArchiveSuffix :: Has (Lift IO) sig m => Path Rel File -> m (Path Rel Dir)
convertArchiveSuffix file = do
  -- Lifting this exception into IO is not exactly safe, but since it's coming directly from a filename this should never error.
  name <- sendIO . P.parseRelDir $ P.toFilePath (P.filename file) <> "!_fossa.virtual_!"
  pure $ P.parent file </> name

-- | Wait for analysis to complete
waitForAnalysis ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  ) =>
  ApiOpts ->
  VSI.ScanID ->
  m ()
waitForAnalysis apiOpts scanID = do
  status <- vsiScanAnalysisStatus apiOpts scanID
  case status of
    VSI.AnalysisFailed ->
      fatalText "Backend analysis failed. If this persists, please contact FOSSA and provide debug logs (generated with --debug)"
    VSI.AnalysisFinished ->
      logDebug "Backend analysis complete"
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

updateProgress :: Has StickyLogger sig m => Text -> Progress -> m ()
updateProgress status Progress{..} =
  logSticky'
    ( renderStatus <> "[ "
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
