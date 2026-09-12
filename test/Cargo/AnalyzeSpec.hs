{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Stage 3 tests for wiring the lockfile-first behavior into
-- "Strategy.Cargo":
--
--   * the pure source-selection decisions (full analysis and static
--     analysis);
--   * 'Strategy.Cargo.analyze' building the graph directly from a
--     present, parsable 'Cargo.lock' (no @cargo@ invocation);
--   * 'Strategy.Cargo.readLockfileSoft' surfacing an unreadable lockfile as a
--     read error (the input to 'analyze's warn + fallback branch).
module Cargo.AnalyzeSpec (
  spec,
) where

import Control.Effect.Lift (sendIO)
import Data.Set qualified as Set
import DepTypes
import Effect.ReadFS (ReadFSErr (FileReadError))
import GraphUtil (expectDeps', expectDirect')
import Path (mkRelFile, toFilePath, (</>))
import Strategy.Cargo (
  CargoProject (..),
  analyze,
  readLockfileSoft,
 )
import Strategy.CargoLock qualified as Lock
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)
import Test.Effect (expectationFailure', itWithTempDir', shouldBe', shouldSatisfy')
import Test.Hspec (Spec, describe, it, shouldBe)
import Types (GraphBreadth (Complete))

-- | A single-crate manifest with one registry dependency.
demoManifest :: String
demoManifest =
  unlines
    [ "[package]"
    , "name = \"demo\""
    , "version = \"0.1.0\""
    , ""
    , "[dependencies]"
    , "serde = \"1\""
    ]

-- | A parsable lockfile for 'demoManifest'.
demoLock :: String
demoLock =
  unlines
    [ "version = 3"
    , ""
    , "[[package]]"
    , "name = \"demo\""
    , "version = \"0.1.0\""
    , "dependencies = [\"serde 1.0.100\"]"
    , ""
    , "[[package]]"
    , "name = \"serde\""
    , "version = \"1.0.100\""
    , "source = \"registry+https://github.com/rust-lang/crates.io-index\""
    , "checksum = \"abcd\""
    ]

demoDependency :: Dependency
demoDependency =
  Dependency
    { dependencyType = CargoType
    , dependencyName = "serde"
    , dependencyVersion = Just $ CEq "1.0.100"
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = mempty
    }

-- ===========================================================================
-- Pure source-selection decisions

sourceDecisionSpecs :: Spec
sourceDecisionSpecs =
  describe "lockfile source decisions" $ do
    let dummyLock = Lock.CargoLock 3 [] []
    describe "analysisLockfileSource (full analysis)" $ do
      it "chooses the metadata path when no lockfile exists" $
        Lock.analysisLockfileSource Nothing `shouldBe` Lock.UseMetadata
      it "chooses the lockfile path when the lockfile parses" $
        Lock.analysisLockfileSource (Just $ Right dummyLock) `shouldBe` Lock.UseLockfile
      it "chooses the metadata path when the lockfile is corrupt" $
        Lock.analysisLockfileSource (Just $ Left (Lock.TomlParseError "boom")) `shouldBe` Lock.UseMetadata
      it "chooses the metadata path on an unsupported lockfile version" $
        Lock.analysisLockfileSource (Just $ Left (Lock.UnsupportedVersion 2)) `shouldBe` Lock.UseMetadata
    describe "staticLockfileOutcome (static analysis)" $ do
      it "reports a missing lockfile" $
        Lock.staticLockfileOutcome Nothing `shouldBe` Lock.StaticMissingLockfile
      it "reports an unparseable lockfile" $
        Lock.staticLockfileOutcome (Just $ Left (Lock.TomlParseError "boom")) `shouldBe` Lock.StaticUnparseableLockfile (Lock.TomlParseError "boom")
      it "reports a parsable lockfile" $
        Lock.staticLockfileOutcome (Just $ Right dummyLock) `shouldBe` Lock.StaticUseLockfile dummyLock

-- ===========================================================================
-- Full (dynamic) analysis wiring

fullAnalysisSpecs :: Spec
fullAnalysisSpecs =
  describe "analyze (lockfile-first)" $ do
    itWithTempDir' "builds the graph from Cargo.lock without running cargo" $ \dir -> do
      let tomlFile = dir </> $(mkRelFile "Cargo.toml")
          lockFile = dir </> $(mkRelFile "Cargo.lock")
      sendIO $ writeFile (toFilePath tomlFile) demoManifest
      sendIO $ writeFile (toFilePath lockFile) demoLock
      let project = CargoProject dir tomlFile
      (graph, breadth) <- analyze False project
      breadth `shouldBe'` Complete
      expectDeps' [demoDependency] graph
      expectDirect' [demoDependency] graph

-- ===========================================================================
-- Dynamic-mode lockfile reading (read-error tolerance)

lockfileReadSpecs :: Spec
lockfileReadSpecs =
  describe "readLockfileSoft (dynamic analysis)" $ do
    itWithTempDir' "returns Right Nothing when no Cargo.lock is present" $ \dir -> do
      res <- readLockfileSoft dir
      res `shouldBe'` Right Nothing
    itWithTempDir' "returns the parsed lockfile when a parsable Cargo.lock is present" $ \dir -> do
      let lockFile = dir </> $(mkRelFile "Cargo.lock")
      sendIO $ writeFile (toFilePath lockFile) demoLock
      res <- readLockfileSoft dir
      case res of
        Right (Just (Right lock)) -> do
          Lock.lockVersion lock `shouldBe'` 3
          map Lock.packageName (Lock.lockPackages lock) `shouldBe'` ["demo", "serde"]
        other -> expectationFailure' ("expected a parsed lockfile, got: " ++ show other)
    itWithTempDir' "surfaces Left with the read error when Cargo.lock cannot be read" $ \dir -> do
      -- The warn + cargo-metadata fallback in 'analyze' itself is not unit
      -- tested: taking it would run cargo. This pins the Left branch that
      -- feeds it, using a file that exists but is unreadable.
      let lockFile = dir </> $(mkRelFile "Cargo.lock")
      sendIO $ writeFile (toFilePath lockFile) demoLock
      sendIO $ setFileMode (toFilePath lockFile) (0 :: FileMode)
      res <- readLockfileSoft dir
      case res of
        Left (FileReadError path reason) -> do
          path `shouldBe'` toFilePath lockFile
          reason `shouldSatisfy'` (/= "")
        other -> expectationFailure' ("expected a read error, got: " ++ show other)

spec :: Spec
spec = do
  sourceDecisionSpecs
  fullAnalysisSpecs
  lockfileReadSpecs
