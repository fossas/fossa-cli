{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.AnalyzeSpec (spec) where

import App.Fossa.Analyze (applyFiltersToProject, runAnalyzers, sendToDestination)
import App.Fossa.Analyze.Discover (DiscoverFunc, discoverFuncs)
import App.Fossa.Analyze.Types (DiscoveredProjectIdentifier (dpiProjectPath), DiscoveredProjectScan (..))
import App.Fossa.Config.Analyze (AnalysisTacticTypes (StaticOnly), StrategyConfig (..), UseGitBackedCargoLocators (UseGitBackedCargoLocators))
import App.Fossa.Config.Common (DestinationMeta (DestinationMeta), ScanDestination (..))
import App.Types (Mode (NonStrict), OverrideDynamicAnalysisBinary (OverrideDynamicAnalysisBinary))
import App.Util (FileAncestry (FileAncestry), ancestryDerived, ancestryDirect)
import Control.Carrier.AtomicCounter (runAtomicCounter)
import Control.Carrier.Debug (DebugC)
import Control.Carrier.Diagnostics (DiagnosticsC)
import Control.Carrier.Output.IO (runOutput)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Stack (StackC)
import Control.Carrier.State.Strict (runState)
import Control.Carrier.TaskPool (withTaskPool)
import Control.Carrier.Telemetry (TelemetryC, withoutTelemetry)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (sendIO)
import Control.Effect.State (modify)
import Data.Aeson qualified as Aeson
import Data.Flag (toFlag')
import Data.Foldable (for_)
import Data.Glob qualified as Glob
import Data.IORef (newIORef)
import Data.List (sort)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Discovery.Archive (convertArchiveToDir, mkZip)
import Discovery.Archive qualified as Archive
import Discovery.Filters (AllFilters (AllFilters), MavenScopeFilters (MavenScopeExcludeFilters), comboExclude, comboExcludeWithGlobs, comboInclude)
import Effect.Exec (ExecIOC)
import Effect.Logger (LoggerC)
import Effect.ReadFS (ReadFSIOC)
import Path (Abs, Dir, File, Path, Rel, dirname, mkAbsDir, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import Test.Effect (EffectStack, it', shouldBe', withTempDir)
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, shouldBe)
import Type.Operator (type ($))
import Types (DiscoveredProject (..), DiscoveredProjectType (MavenProjectType), FoundTargets (ProjectWithoutTargets))

type SomeMonad = TelemetryC $ ReaderC OverrideDynamicAnalysisBinary $ ReaderC StrategyConfig $ ReaderC MavenScopeFilters $ ReaderC Mode $ ReaderC AllFilters $ DebugC $ DiagnosticsC $ LoggerC $ ExecIOC $ ReadFSIOC $ StackC IO

spec :: Spec
spec = do
  -- this test only exists to prevent merging the commented out analyzers
  describe "Discovery function list" $
    it "should be length 39" $
      length (discoverFuncs :: [DiscoverFunc SomeMonad]) `shouldBe` 39

  sendToDestinationSpec

  describe "applyFiltersToProject" $ do
    describe "projects under the scan basedir" $ do
      it "excludes a project matching an exclusion filter" $
        applyFiltersToProject scanRoot Nothing (excluding $(mkRelDir "third-party")) (project . inScan $ $(mkRelDir "third-party/lib")) `shouldBe` Nothing

      it "keeps a project not matching any exclusion filter" $
        applyFiltersToProject scanRoot Nothing (excluding $(mkRelDir "third-party")) (project . inScan $ $(mkRelDir "app")) `shouldBe` Just ProjectWithoutTargets

      it "keeps a project matching an inclusion filter" $
        applyFiltersToProject scanRoot Nothing (including $(mkRelDir "app")) (project . inScan $ $(mkRelDir "app/server")) `shouldBe` Just ProjectWithoutTargets

      it "excludes a project not matching an inclusion filter" $
        applyFiltersToProject scanRoot Nothing (including $(mkRelDir "app")) (project . inScan $ $(mkRelDir "third-party/lib")) `shouldBe` Nothing

      -- The walk prunes folders matching an exclusion glob before it reaches
      -- the projects inside them.
      it "matches exclusion globs against the project's own path only" $ do
        applyFiltersToProject scanRoot Nothing (excludingGlob "bundles/*") (project . inScan $ $(mkRelDir "bundles/lib")) `shouldBe` Nothing
        applyFiltersToProject scanRoot Nothing (excludingGlob "bundles/*") (project . inScan $ $(mkRelDir "bundles/lib/app")) `shouldBe` Just ProjectWithoutTargets

    -- Archives are unpacked to a temp directory, so filters have to be applied
    -- to the archive's path in the scan (carried by the 'FileAncestry' prefix)
    -- rather than to the temp path the contents actually live at.
    describe "projects inside an unpacked archive" $ do
      it "excludes archive contents when the archive's path in the scan is excluded" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "third-party/lib.zip")) (excluding $(mkRelDir "third-party")) (project unpackedProject) `shouldBe` Nothing

      it "excludes archive contents when a directory inside the archive is excluded" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "third-party/lib.zip")) (excluding $(mkRelDir "third-party/lib.zip/maven-project")) (project unpackedProject) `shouldBe` Nothing

      it "keeps archive contents when no filter matches the archive's path in the scan" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "vendor/lib.zip")) (excluding $(mkRelDir "third-party")) (project unpackedProject) `shouldBe` Just ProjectWithoutTargets

      it "keeps archive contents when no filters are configured" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "third-party/lib.zip")) mempty (project unpackedProject) `shouldBe` Just ProjectWithoutTargets

      it "keeps archive contents matching an inclusion filter" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "vendor/lib.zip")) (including $(mkRelDir "vendor")) (project unpackedProject) `shouldBe` Just ProjectWithoutTargets

      it "excludes archive contents not matching an inclusion filter" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "third-party/lib.zip")) (including $(mkRelDir "vendor")) (project unpackedProject) `shouldBe` Nothing

      it "excludes archive contents when an exclusion glob matches the archive's path in the scan" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "bundles/lib.zip")) (excludingGlob "bundles/*.zip") (project unpackedProject) `shouldBe` Nothing

    -- Unpacking is recursive: an archive inside an archive gets an ancestry
    -- prefix accumulating every archive between it and the scan root.
    describe "projects inside a nested archive" $ do
      it "excludes nested archive contents when an ancestor directory is excluded" $
        applyFiltersToProject nestedUnpackedRoot (ancestry $(mkRelDir "third-party/outer.zip/nested/inner.zip")) (excluding $(mkRelDir "third-party")) (project nestedUnpackedProject) `shouldBe` Nothing

      it "excludes nested archive contents when the outer archive is excluded" $
        applyFiltersToProject nestedUnpackedRoot (ancestry $(mkRelDir "third-party/outer.zip/nested/inner.zip")) (excluding $(mkRelDir "third-party/outer.zip")) (project nestedUnpackedProject) `shouldBe` Nothing

      it "keeps nested archive contents when no filter matches" $
        applyFiltersToProject nestedUnpackedRoot (ancestry $(mkRelDir "vendor/outer.zip/nested/inner.zip")) (excluding $(mkRelDir "third-party")) (project nestedUnpackedProject) `shouldBe` Just ProjectWithoutTargets

      it "excludes nested archive contents when an exclusion glob matches the outer archive" $
        applyFiltersToProject nestedUnpackedRoot (ancestry $(mkRelDir "bundles/outer.zip/nested/inner.zip")) (excludingGlob "bundles/*.zip") (project nestedUnpackedProject) `shouldBe` Nothing

  -- Runs @--unpack-archives@ discovery end to end, so filters are checked both
  -- while walking an archive's contents and on each project found in them.
  describe "projects found by unpacking archives" $ do
    it' "keeps the projects in an archive under an included folder" $ do
      found <- scanArchive $(mkRelDir "vendor") (including $(mkRelDir "vendor"))
      found `shouldBe'` [($(mkRelDir "app"), "scanned"), ($(mkRelDir "dep"), "scanned")]

    it' "keeps the Pixi projects in an archive under an included folder" $ do
      found <- scanArchiveWith $(mkRelFile "pixi.lock") "version: 6\nenvironments: {}\npackages: []\n" $(mkRelDir "vendor") (including $(mkRelDir "vendor"))
      found `shouldBe'` [($(mkRelDir "app"), "scanned"), ($(mkRelDir "dep"), "scanned")]

    it' "doesn't exclude a folder inside an archive by its path inside the archive" $ do
      found <- scanArchive $(mkRelDir "vendor") (excluding $(mkRelDir "app"))
      found `shouldBe'` [($(mkRelDir "app"), "scanned"), ($(mkRelDir "dep"), "scanned")]

    it' "skips the projects in an archive matching an exclusion glob" $ do
      found <- scanArchive $(mkRelDir "bundles") (excludingGlob "bundles/*.zip")
      found `shouldBe'` [($(mkRelDir "app"), "skipped"), ($(mkRelDir "dep"), "skipped")]

    it' "prunes a folder inside an archive matching an exclusion glob" $ do
      found <- scanArchive $(mkRelDir "bundles") (excludingGlob "**/vendor")
      found `shouldBe'` [($(mkRelDir "app"), "scanned")]
      foundByPathInScan <- scanArchive $(mkRelDir "bundles") (excludingGlob "bundles/*.zip/vendor")
      foundByPathInScan `shouldBe'` [($(mkRelDir "app"), "scanned")]

  -- The tests above hand 'applyFiltersToProject' an already-correct prefix.
  -- These pin down the code that derives it, so that a regression there is not
  -- invisible to this spec.
  --
  -- The scan is rooted at @<fsRoot>/third-party/user@ with the archive at
  -- @<fsRoot>/third-party/user/project/archive.tar@, so @third-party@ and
  -- @user@ sit above the scan root: the user cannot see them and no filter of
  -- theirs may match on them.
  describe "archive path prefix derivation" $ do
    it' "is the archive's path relative to the scan basedir" $ do
      prefix <- archivePrefix archiveScanRoot outerArchive
      prefix `shouldBe'` $(mkRelDir "project/archive.tar")

    it' "accumulates the inner archive's path onto the outer archive's prefix" $ do
      outerPrefix <- archivePrefix archiveScanRoot outerArchive
      prefix <- nestedArchivePrefix (FileAncestry outerPrefix) unpackedRoot innerArchive
      prefix `shouldBe'` $(mkRelDir "project/archive.tar/nested/inner.zip")

    it' "excludes archive contents via a directory between the basedir and the archive" $ do
      prefix <- archivePrefix archiveScanRoot outerArchive
      applyFiltersToProject unpackedRoot (ancestry prefix) (excluding $(mkRelDir "project")) (project unpackedProject) `shouldBe'` Nothing

    it' "keeps archive contents when the excluded directory sits above the scan basedir" $ do
      prefix <- archivePrefix archiveScanRoot outerArchive
      applyFiltersToProject unpackedRoot (ancestry prefix) (excluding $(mkRelDir "third-party")) (project unpackedProject) `shouldBe'` Just ProjectWithoutTargets
      applyFiltersToProject unpackedRoot (ancestry prefix) (excluding $(mkRelDir "user")) (project unpackedProject) `shouldBe'` Just ProjectWithoutTargets

-- | Runs under the mock API with no expectations, so any request the stdout
-- branch made would fail the test on top of the upload count.
sendToDestinationSpec :: Spec
sendToDestinationSpec = describe "sendToDestination" $ do
  let meta = DestinationMeta (Fixtures.apiOpts, Fixtures.projectMetadata)
      result = Aeson.object []
      countUploads destination = runState (0 :: Int) $ sendToDestination (\_ -> modify ((+ 1) :: Int -> Int)) destination result

  it' "uploads nothing when the destination is stdout" $ do
    (uploads, ()) <- countUploads OutputStdout
    uploads `shouldBe'` 0

  it' "uploads once for an upload destination" $ do
    (uploads, ()) <- countUploads (UploadScan meta)
    uploads `shouldBe'` 1

  it' "uploads once when also writing to stdout" $ do
    (uploads, ()) <- countUploads (OutputAndUpload meta)
    uploads `shouldBe'` 1

-- | The prefix 'Discovery.Archive.discover' builds for an archive found
-- directly under the scan basedir, as assembled at its callsite in
-- "App.Fossa.Analyze".
archivePrefix :: Has Diagnostics sig m => Path Abs Dir -> Path Abs File -> m (Path Rel Dir)
archivePrefix basedir archive = ancestryDirect basedir archive >>= convertArchiveToDir

-- | The prefix 'Discovery.Archive.discover' builds for an archive found inside
-- another archive's unpacked contents.
nestedArchivePrefix :: Has Diagnostics sig m => FileAncestry -> Path Abs Dir -> Path Abs File -> m (Path Rel Dir)
nestedArchivePrefix parent basedir archive = ancestryDerived parent basedir archive >>= convertArchiveToDir

-- | The filesystem root every absolute path below is anchored to. Windows
-- rejects absolute paths without a drive letter at compile time, so this is the
-- only binding in the file that needs to be platform-specific; everything under
-- it is relative and so parses identically on both platforms.
fsRoot :: Path Abs Dir
#ifdef mingw32_HOST_OS
fsRoot = $(mkAbsDir "C:/")
#else
fsRoot = $(mkAbsDir "/")
#endif

scanRoot :: Path Abs Dir
scanRoot = fsRoot </> $(mkRelDir "scan")

-- | The temp directory an archive's contents are unpacked into. This is the
-- basedir discovery walks for archive contents.
unpackedRoot :: Path Abs Dir
unpackedRoot = fsRoot </> $(mkRelDir "tmp/lib.zip-abc123")

-- | The temp directory the inner archive of a nested archive is unpacked into.
nestedUnpackedRoot :: Path Abs Dir
nestedUnpackedRoot = fsRoot </> $(mkRelDir "tmp/inner.zip-def456")

-- | The scan basedir for the prefix-derivation tests. @third-party@ and @user@
-- are real directories on disk, but the scan is rooted at @user@, so they sit
-- above the basedir and must never appear in a derived prefix.
archiveScanRoot :: Path Abs Dir
archiveScanRoot = fsRoot </> $(mkRelDir "third-party/user")

-- | An archive one directory below 'archiveScanRoot'. The intervening
-- @project@ directory must survive into the prefix.
outerArchive :: Path Abs File
outerArchive = archiveScanRoot </> $(mkRelFile "project/archive.tar")

-- | An archive inside 'outerArchive', found by walking its unpacked contents
-- at 'unpackedRoot'.
innerArchive :: Path Abs File
innerArchive = unpackedRoot </> $(mkRelFile "nested/inner.zip")

-- | A project discovered directly under 'scanRoot'.
inScan :: Path Rel Dir -> Path Abs Dir
inScan = (scanRoot </>)

-- | The project discovered inside the unpacked archive at 'unpackedRoot'.
unpackedProject :: Path Abs Dir
unpackedProject = unpackedRoot </> $(mkRelDir "maven-project")

-- | The project discovered inside the unpacked nested archive at 'nestedUnpackedRoot'.
nestedUnpackedProject :: Path Abs Dir
nestedUnpackedProject = nestedUnpackedRoot </> $(mkRelDir "maven-project")

project :: Path Abs Dir -> DiscoveredProject ()
project path = DiscoveredProject MavenProjectType path ProjectWithoutTargets ()

ancestry :: Path Rel Dir -> Maybe FileAncestry
ancestry = Just . FileAncestry

excluding :: Path Rel Dir -> AllFilters
excluding path = AllFilters mempty (comboExclude mempty [path])

excludingGlob :: String -> AllFilters
excludingGlob glob = AllFilters mempty (comboExcludeWithGlobs mempty mempty [Glob.unsafeGlobRel glob])

-- | Runs @--unpack-archives@ discovery the way 'App.Fossa.Analyze.analyze'
-- does, over a scan holding @lib.zip@ in @folder@, with pip projects at @app/@
-- and @vendor/dep/@ inside the archive. Returns each project found, by its
-- folder name, and whether it was scanned. Default filters are off, so only
-- @filters@ decide.
scanArchive :: Path Rel Dir -> AllFilters -> EffectStack [(Path Rel Dir, Text)]
scanArchive = scanArchiveWith $(mkRelFile "requirements.txt") "six==1.16.0\n"

-- | Like 'scanArchive', with @manifest@ holding @manifestContents@ in each
-- project folder instead of a pip @requirements.txt@.
scanArchiveWith :: Path Rel File -> String -> Path Rel Dir -> AllFilters -> EffectStack [(Path Rel Dir, Text)]
scanArchiveWith manifest manifestContents folder filters = withTempDir "test-App-Fossa-Analyze-archive" $ \tmp -> do
  let contents = tmp </> $(mkRelDir "contents")
      scan = tmp </> $(mkRelDir "scan")
      archive = scan </> folder </> $(mkRelFile "lib.zip")
  sendIO . for_ [$(mkRelDir "app"), $(mkRelDir "vendor/dep")] $ \dir -> do
    createDirIfMissing True (contents </> dir)
    writeFile (toFilePath (contents </> dir </> manifest)) manifestContents
  sendIO $ createDirIfMissing True (scan </> folder)
  mkZip contents archive
  -- The discovery span ref is unused by this test; it only exists so 'runAnalyzers'
  -- can fold discovery-only wall-clock time into telemetry.
  spanRef <- sendIO (newIORef (Nothing :: Maybe (UTCTime, UTCTime)))
  (scans, ()) <-
    runOutput @DiscoveredProjectScan
      . withTaskPool 1 (const (pure ()))
      . runAtomicCounter
      . withoutTelemetry
      . runReader strategyConfig
      . runReader (MavenScopeExcludeFilters mempty)
      . runReader filters
      . runReader (OverrideDynamicAnalysisBinary mempty)
      . runReader NonStrict
      $ Archive.discover
        ( \dir pathPrefix ->
            runAnalyzers StaticOnly filters (toFlag' True) dir pathPrefix spanRef
        )
        scan
        ancestryDirect
  pure . sort $ map outcome scans
  where
    strategyConfig =
      StrategyConfig
        { allowedGradleConfigs = Nothing
        , resolvePathDependencies = False
        , useGitBackedCargoLocators = UseGitBackedCargoLocators False
        , enableGoBinaryAnalysis = False
        }
    outcome :: DiscoveredProjectScan -> (Path Rel Dir, Text)
    outcome (Scanned dpi _) = (dirname (dpiProjectPath dpi), "scanned")
    outcome (SkippedDueToProvidedFilter dpi) = (dirname (dpiProjectPath dpi), "skipped")
    outcome (SkippedDueToDefaultFilter dpi) = (dirname (dpiProjectPath dpi), "skipped by default filters")

including :: Path Rel Dir -> AllFilters
including path = AllFilters (comboInclude mempty [path]) mempty
