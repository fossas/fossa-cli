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
import Control.Concurrent (threadDelay)
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, fromEither)
import Control.Effect.Finally (Finally)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.StickyLogger (StickyLogger, logSticky)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Discovery.Archive (withArchive')
import Discovery.Filters (AllFilters, combinedPaths, excludeFilters, includeFilters)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipAll), walk')
import Effect.Logger (Logger, logDebug, pretty)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Path (Abs, Dir, File, Path, Rel, SomeBase (Abs, Rel), isProperPrefixOf, (</>))
import Path qualified as P
import Path.Extra (renderRelative, tryMakeRelative)

type FingerprintEffs sig m =
  ( Has (Lift IO) sig m
  , Has Finally sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  )

runVsiAnalysis ::
  ( FingerprintEffs sig m
  , Has StickyLogger sig m
  ) =>
  Path Abs Dir ->
  ApiOpts ->
  ProjectRevision ->
  AllFilters ->
  m ([VSI.Locator], [IAT.UserDep])
runVsiAnalysis dir apiOpts projectRevision filters = context "VSI" $ do
  -- TODO(kit): Figure out how to parallelize fingerprinting
  -- TODO(kit): Figure out how to stream fingerprinting
  fingerprints <- context "Fingerprint files" $ runFingerprint (toPathFilters dir filters) dir
  when (Map.null fingerprints) $ fatalText "No files fingerprinted"

  scanID <- context "Create scan in backend" $ vsiCreateScan apiOpts projectRevision
  logDebug . pretty $ "Created Scan ID: " <> unScanID scanID

  -- Split into 1000-fingerprint buckets for uploading.
  -- This number wasn't chosen for any specific reason, it's just what the current VSI plugin does.
  -- The goal is to ensure that we don't hit any upload size limits.
  let chunks = map Map.fromList . chunksOf 1000 $ Map.toList fingerprints
  logDebug . pretty $
    "Adding "
      <> toText (show $ length fingerprints)
      <> " file(s) to scan "
      <> VSI.unScanID scanID
      <> " in "
      <> toText (show $ length chunks)
      <> " chunk(s)"
  context "Upload fingerprints" $ traverse_ (uploadChunk scanID) chunks

  context "Finalize scan files" $ vsiCompleteScan apiOpts scanID
  context "Waiting for backend analysis to complete" $ waitForAnalysis apiOpts scanID

  discoveredRawLocators <- context "Download analysis results" $ vsiDownloadInferences apiOpts scanID
  parsedLocators <- context "Parse analysis results" . fromEither $ traverse VSI.parseLocator discoveredRawLocators

  let userDefinedDeps = map IAT.toUserDep $ filter VSI.isUserDefined parsedLocators
  let allOtherDeps = filter (not . VSI.isUserDefined) parsedLocators
  pure (allOtherDeps, userDefinedDeps)
  where
    uploadChunk scanID chunk = do
      logDebug . pretty $ "Uploading chunk of " <> toText (show $ length chunk) <> " fingerprints:"
      traverse_ (logDebug . pretty . ("  " <>) . toText . P.toFilePath . fst) $ Map.toList chunk
      vsiAddFilesToScan apiOpts scanID chunk

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
runFingerprint :: FingerprintEffs sig m => PathFilters -> Path Abs Dir -> m (Map (Path Rel File) Combined)
runFingerprint filters root = flip walk' root $ \dir _ files -> do
  if filters `allow` dir
    then do
      fingerprints <-
        context ("Fingerprint directory contents: " <> renderRelative root dir) $
          traverse (fingerprintFile filters root) files
      pure (Map.unions fingerprints, WalkContinue)
    else do
      logDebug . pretty $ "Excluded by filter: " <> toText (show dir)
      pure (Map.empty, WalkSkipAll)

-- | Fingerprints the file, and attempts to extract it as though it was an archive.
--
-- If the file is an archive, its contents are extracted and fingerprinted, resulting in a map
-- of the fingerprint of the archive itself combined with the fingerprints of its contents
-- (with remapped file paths, per the comment for @runFingerprint@).
--
-- If the file is not an archive, results in a singleton map of the fingerprints for this file.
fingerprintFile :: FingerprintEffs sig m => PathFilters -> Path Abs Dir -> Path Abs File -> m (Map (Path Rel File) Combined)
fingerprintFile filters root file = do
  let desc = renderRelative root file
  logDebug . pretty $ "Fingerprint: " <> desc

  relFile <- mustMakeRelative root file
  fp <- context ("fingerprint: " <> desc) $ fingerprint file
  let direct = Map.singleton relFile fp

  contains <- context ("extract as archive: " <> desc) $ withArchive' file (runFingerprint filters)
  case contains of
    -- In the most common case where this file was not an archive, evaluate to the singleton directly.
    Nothing -> pure direct
    -- In the case where `file` is an archive, we'll have a map of paths relative to the archive.
    -- Remap the ancestry of all the contained files such that they are reported correctly.
    Just contents -> do
      withRemappedAncestry <-
        context ("remap contents into virtual paths: " <> desc) $
          traverse (remapAncestry relFile) $
            Map.toList contents
      pure . Map.union direct $ Map.fromList withRemappedAncestry
  where
    remapAncestry archive (target, fp) = do
      parent <- convertArchiveSuffix archive
      pure (parent </> target, fp)

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

-- | Makes the passed file relative to the passed directory.
-- If that's not possible, fatally exits via the Diagnostics effect.
mustMakeRelative :: Has Diagnostics sig m => Path Abs Dir -> Path Abs File -> m (Path Rel File)
mustMakeRelative root file = case tryMakeRelative root file of
  Abs _ -> fatalText $ "cannot make " <> toText (show file) <> " relative to " <> toText (show root)
  Rel rel -> pure rel

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
