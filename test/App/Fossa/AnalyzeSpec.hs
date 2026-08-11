{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.AnalyzeSpec (spec) where

import App.Fossa.Analyze (applyFiltersToProject)
import App.Fossa.Analyze.Discover (DiscoverFunc, discoverFuncs)
import App.Fossa.Config.Analyze (StrategyConfig)
import App.Types (Mode, OverrideDynamicAnalysisBinary)
import App.Util (FileAncestry (FileAncestry))
import Control.Carrier.Debug (DebugC)
import Control.Carrier.Diagnostics (DiagnosticsC)
import Control.Carrier.Reader (ReaderC)
import Control.Carrier.Stack (StackC)
import Control.Carrier.Telemetry (TelemetryC)
import Discovery.Filters (AllFilters (AllFilters), MavenScopeFilters, comboExclude, comboInclude)
import Effect.Exec (ExecIOC)
import Effect.Logger (LoggerC)
import Effect.ReadFS (ReadFSIOC)
import Path (Abs, Dir, Path, Rel, mkAbsDir, mkRelDir)
import Test.Hspec (Spec, describe, it, shouldBe)
import Type.Operator (type ($))
import Types (DiscoveredProject (..), DiscoveredProjectType (MavenProjectType), FoundTargets (ProjectWithoutTargets))

type SomeMonad = TelemetryC $ ReaderC OverrideDynamicAnalysisBinary $ ReaderC StrategyConfig $ ReaderC MavenScopeFilters $ ReaderC Mode $ ReaderC AllFilters $ DebugC $ DiagnosticsC $ LoggerC $ ExecIOC $ ReadFSIOC $ StackC IO

spec :: Spec
spec = do
  -- this test only exists to prevent merging the commented out analyzers
  describe "Discovery function list" $
    it "should be length 36" $
      length (discoverFuncs :: [DiscoverFunc SomeMonad]) `shouldBe` 36

  describe "applyFiltersToProject" $ do
    describe "projects under the scan basedir" $ do
      it "excludes a project matching an exclusion filter" $
        applyFiltersToProject scanRoot Nothing (excluding $(mkRelDir "third-party")) (project $(mkAbsDir "/scan/third-party/lib")) `shouldBe` Nothing

      it "keeps a project not matching any exclusion filter" $
        applyFiltersToProject scanRoot Nothing (excluding $(mkRelDir "third-party")) (project $(mkAbsDir "/scan/app")) `shouldBe` Just ProjectWithoutTargets

      it "keeps a project matching an inclusion filter" $
        applyFiltersToProject scanRoot Nothing (including $(mkRelDir "app")) (project $(mkAbsDir "/scan/app/server")) `shouldBe` Just ProjectWithoutTargets

      it "excludes a project not matching an inclusion filter" $
        applyFiltersToProject scanRoot Nothing (including $(mkRelDir "app")) (project $(mkAbsDir "/scan/third-party/lib")) `shouldBe` Nothing

    -- Archives are unpacked to a temp directory, so filters have to be applied
    -- to the archive's path in the scan (carried by the 'FileAncestry' prefix)
    -- rather than to the temp path the contents actually live at.
    describe "projects inside an unpacked archive" $ do
      it "excludes archive contents when the archive's path in the scan is excluded" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "third-party/lib.zip")) (excluding $(mkRelDir "third-party")) (project $(mkAbsDir "/tmp/lib.zip-abc123/maven-project")) `shouldBe` Nothing

      it "excludes archive contents when a directory inside the archive is excluded" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "third-party/lib.zip")) (excluding $(mkRelDir "third-party/lib.zip/maven-project")) (project $(mkAbsDir "/tmp/lib.zip-abc123/maven-project")) `shouldBe` Nothing

      it "keeps archive contents when no filter matches the archive's path in the scan" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "vendor/lib.zip")) (excluding $(mkRelDir "third-party")) (project $(mkAbsDir "/tmp/lib.zip-abc123/maven-project")) `shouldBe` Just ProjectWithoutTargets

      it "keeps archive contents when no filters are configured" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "third-party/lib.zip")) mempty (project $(mkAbsDir "/tmp/lib.zip-abc123/maven-project")) `shouldBe` Just ProjectWithoutTargets

      it "keeps archive contents matching an inclusion filter" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "vendor/lib.zip")) (including $(mkRelDir "vendor")) (project $(mkAbsDir "/tmp/lib.zip-abc123/maven-project")) `shouldBe` Just ProjectWithoutTargets

      it "excludes archive contents not matching an inclusion filter" $
        applyFiltersToProject unpackedRoot (ancestry $(mkRelDir "third-party/lib.zip")) (including $(mkRelDir "vendor")) (project $(mkAbsDir "/tmp/lib.zip-abc123/maven-project")) `shouldBe` Nothing

    -- Unpacking is recursive: an archive inside an archive gets an ancestry
    -- prefix accumulating every archive between it and the scan root.
    describe "projects inside a nested archive" $ do
      it "excludes nested archive contents when an ancestor directory is excluded" $
        applyFiltersToProject nestedUnpackedRoot (ancestry $(mkRelDir "third-party/outer.zip/nested/inner.zip")) (excluding $(mkRelDir "third-party")) (project $(mkAbsDir "/tmp/inner.zip-def456/maven-project")) `shouldBe` Nothing

      it "excludes nested archive contents when the outer archive is excluded" $
        applyFiltersToProject nestedUnpackedRoot (ancestry $(mkRelDir "third-party/outer.zip/nested/inner.zip")) (excluding $(mkRelDir "third-party/outer.zip")) (project $(mkAbsDir "/tmp/inner.zip-def456/maven-project")) `shouldBe` Nothing

      it "keeps nested archive contents when no filter matches" $
        applyFiltersToProject nestedUnpackedRoot (ancestry $(mkRelDir "vendor/outer.zip/nested/inner.zip")) (excluding $(mkRelDir "third-party")) (project $(mkAbsDir "/tmp/inner.zip-def456/maven-project")) `shouldBe` Just ProjectWithoutTargets

scanRoot :: Path Abs Dir
scanRoot = $(mkAbsDir "/scan")

-- | The temp directory an archive's contents are unpacked into. This is the
-- basedir discovery walks for archive contents.
unpackedRoot :: Path Abs Dir
unpackedRoot = $(mkAbsDir "/tmp/lib.zip-abc123")

-- | The temp directory the inner archive of a nested archive is unpacked into.
nestedUnpackedRoot :: Path Abs Dir
nestedUnpackedRoot = $(mkAbsDir "/tmp/inner.zip-def456")

project :: Path Abs Dir -> DiscoveredProject ()
project path = DiscoveredProject MavenProjectType path ProjectWithoutTargets ()

ancestry :: Path Rel Dir -> Maybe FileAncestry
ancestry = Just . FileAncestry

excluding :: Path Rel Dir -> AllFilters
excluding path = AllFilters mempty (comboExclude mempty [path])

including :: Path Rel Dir -> AllFilters
including path = AllFilters (comboInclude mempty [path]) mempty
