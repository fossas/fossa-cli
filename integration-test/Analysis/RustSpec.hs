{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Analysis.RustSpec (spec) where

import Analysis.FixtureExpectationUtils (
  expectProject,
  getDepResultsOf,
  withAnalysisOf,
 )
import Analysis.FixtureUtils (
  AnalysisTestFixture (..),
  FixtureArtifact (..),
  FixtureEnvironment (LocalEnvironment, NixEnv),
  getArtifact,
  performDiscoveryAndAnalyses,
 )
import App.Fossa.Analyze.Types (AnalyzeProject)
import App.Types (Mode (..))
import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Data.Maybe (isJust)
import Data.String.Conversion (toString)
import Path (Abs, Dir, File, Path, mkRelFile, reldir, toFilePath, (</>))
import Path.IO qualified as PIO
import Strategy.Cargo qualified as Cargo
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)
import Types (
  DependencyResults,
  DiscoveredProject,
  DiscoveredProjectType (..),
 )

rustEnv :: FixtureEnvironment
rustEnv = NixEnv ["rustc", "cargo"]

bat :: AnalysisTestFixture (Cargo.CargoProject)
bat =
  AnalysisTestFixture
    "bat"
    Cargo.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/sharkdp/bat/archive/refs/tags/v0.18.3.tar.gz"
      [reldir|rust/bat/|]
      [reldir|bat-0.18.3//|]

fd :: AnalysisTestFixture (Cargo.CargoProject)
fd =
  AnalysisTestFixture
    "fd"
    Cargo.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/sharkdp/fd/archive/refs/tags/v8.3.0.tar.gz"
      [reldir|rust/fd/|]
      [reldir|fd-8.3.0/|]

-- | The bat fixture with the rust toolchain environment, used to exercise
-- the @cargo metadata@ fallback path (its @Cargo.lock@ is deleted before
-- analysis in 'withAnalysisOfAfterLockfileRemoval').
batMetadataFallback :: AnalysisTestFixture (Cargo.CargoProject)
batMetadataFallback =
  bat
    { testName = "bat (metadata fallback)"
    , environment = rustEnv
    }

lockfilePath :: Path Abs Dir -> Path Abs File
lockfilePath projectDir = projectDir </> $(mkRelFile "Cargo.lock")

-- | Run discovery and analysis for a fixture whose analysis is expected to
-- take the @Cargo.lock@ path (no toolchain required, hence
-- 'LocalEnvironment'). Explicitly pins that @Cargo.lock@ is present in the
-- extracted project, so a future tag bump that drops the lockfile from the
-- tarball fails loudly instead of silently exercising the @cargo metadata@
-- path.
lockfileFixtureSuite :: AnalysisTestFixture (Cargo.CargoProject) -> Spec
lockfileFixtureSuite fixture = aroundAll (withAnalysisOf NonStrict fixture) $
  describe (toString $ testName fixture) $ do
    it "extracted fixture includes Cargo.lock" $ \(_, projectDir) -> do
      exists <- sendIO $ PIO.doesFileExist (lockfilePath projectDir)
      exists `shouldBe` True
    it "should find targets" $ \(result, projectDir) ->
      expectProject (CargoProjectType, projectDir) result
    it "should have some dependency results" $ \(result, projectDir) ->
      isJust (getDepResultsOf result (CargoProjectType, projectDir)) `shouldBe` True

-- | Extract the fixture, verify @Cargo.lock@ exists, delete it, then run
-- discovery and analysis. With no lockfile present, the analysis must take
-- the pre-existing @cargo metadata@ path (generating a lockfile first),
-- which requires the rust toolchain environment.
withAnalysisOfAfterLockfileRemoval ::
  (Has (Lift IO) sig m, AnalyzeProject a, MonadFail m) =>
  AnalysisTestFixture a ->
  (([(DiscoveredProject a, DependencyResults)], Path Abs Dir) -> m b) ->
  m ()
withAnalysisOfAfterLockfileRemoval testFixture runTest = do
  extractedDir <- getArtifact (artifact testFixture)
  let projectDir = extractedDir </> scopedDir (artifact testFixture)
  lockExists <- sendIO $ PIO.doesFileExist (lockfilePath projectDir)
  if lockExists
    then sendIO $ PIO.removeFile (lockfilePath projectDir)
    else fail $ "expected Cargo.lock in " <> toFilePath projectDir <> ", but it was not found"
  res <- performDiscoveryAndAnalyses extractedDir testFixture NonStrict
  _ <- runTest (res, projectDir)
  sendIO $ PIO.removeDirRecur extractedDir

metadataFallbackSuite :: AnalysisTestFixture (Cargo.CargoProject) -> Spec
metadataFallbackSuite fixture = aroundAll (withAnalysisOfAfterLockfileRemoval fixture) $
  describe (toString $ testName fixture) $ do
    it "should find targets" $ \(result, projectDir) ->
      expectProject (CargoProjectType, projectDir) result
    it "should have some dependency results" $ \(result, projectDir) ->
      isJust (getDepResultsOf result (CargoProjectType, projectDir)) `shouldBe` True

spec :: Spec
spec = do
  lockfileFixtureSuite bat
  lockfileFixtureSuite fd
  metadataFallbackSuite batMetadataFallback

-- The dependency number can change as the dependency tree for following changes,
-- This is ramification of how `rust` builds are analyzed.
--
-- testSuiteDepResultSummary bat CargoProjectType (DependencyResultsSummary 146 29 268 1 Complete)
-- testSuiteDepResultSummary fd CargoProjectType (DependencyResultsSummary 74 25 145 1 Complete)
