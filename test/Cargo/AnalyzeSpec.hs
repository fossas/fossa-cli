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
--     read error (the input to 'analyze's warn + fallback branch);
--   * 'AnalyzeProject.analyzeProjectStaticOnly' producing real results
--     from a lockfile, and the new fatals for a missing or unparseable
--     lockfile.
module Cargo.AnalyzeSpec (
  spec,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly))
import App.Fossa.Config.Analyze (StrategyConfig (..), UseGitBackedCargoLocators (..))
import App.Types (Mode (..))
import Control.Carrier.Debug (IgnoreDebugC, ignoreDebug)
import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Stack (StackC, runStack)
import Control.Carrier.Telemetry (IgnoreTelemetryC, withoutTelemetry)
import Control.Effect.Lift (sendIO)
import Control.Exception (finally)
import Data.Function ((&))
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import DepTypes
import Diag.Result (EmittedWarn, ErrGroup, Result (Failure, Success), renderFailure)
import Discovery.Filters (AllFilters, MavenScopeFilters (MavenScopeIncludeFilters))
import Effect.Exec (ExecIOC, runExecIO)
import Effect.Logger (LoggerC, Severity (SevWarn), renderIt, withDefaultLogger)
import Effect.ReadFS (ReadFSErr (FileReadError), ReadFSIOC, runReadFSIO)
import GraphUtil (expectDeps', expectDirect')
import Graphing qualified
import Path (Abs, Dir, Path, mkRelFile, parseAbsDir, toFilePath, (</>))
import Strategy.Cargo (
  CargoProject (..),
  analyze,
  readLockfileSoft,
 )
import Strategy.CargoLock qualified as Lock
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)
import System.Random (randomIO)
import Test.Effect (expectationFailure', itWithTempDir', shouldBe', shouldSatisfy')
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldContain)
import Type.Operator (type ($))
import Types (
  DependencyResults (dependencyGraph, dependencyManifestFiles),
  FoundTargets (..),
  GraphBreadth (Complete),
 )

-- | A minimal carrier stack satisfying 'AnalyzeStaticTaskEffs', mirroring the
-- tail of "Analysis.FixtureUtils"' 'TestC' (readers on top, telemetry at the
-- bottom).
type StaticTestC m =
  ReaderC AllFilters
    $ ReaderC Mode
    $ ReaderC MavenScopeFilters
    $ ReaderC StrategyConfig
    $ ExecIOC
    $ ReadFSIOC
    $ IgnoreDebugC
    $ DiagnosticsC
    $ LoggerC
    $ StackC
    $ IgnoreTelemetryC
        m

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

-- | A lockfile that fails to parse (not valid TOML).
corruptLock :: String
corruptLock = "version = "

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

-- ===========================================================================
-- Static analysis wiring

-- | Run a static analysis with a minimal carrier stack. The lockfile
-- branches never use 'Exec'; the real 'Exec' carrier is present but idle.
runStaticAnalysis :: CargoProject -> IO (Result DependencyResults)
runStaticAnalysis project =
  ( analyzeProjectStaticOnly ProjectWithoutTargets project ::
      StaticTestC IO DependencyResults
  )
    & runReader (mempty :: AllFilters)
    & runReader (NonStrict :: Mode)
    & runReader (MavenScopeIncludeFilters mempty)
    & runReader (StrategyConfig Nothing False (UseGitBackedCargoLocators False))
    & runExecIO
    & runReadFSIO
    & ignoreDebug
    & runDiagnostics
    & withDefaultLogger SevWarn
    & runStack
    & withoutTelemetry

renderFailureText :: [EmittedWarn] -> ErrGroup -> Text
renderFailureText ws eg = renderIt (renderFailure ws eg "An issue occurred")

-- | Create a temp directory, run the action in it, and remove it again.
withTempDirIO :: (Path Abs Dir -> IO a) -> IO a
withTempDirIO act = do
  base <- getTemporaryDirectory
  junk :: Word <- randomIO
  let dirStr = base <> "/cargo-analyze-spec-" <> show junk
  dir <- case parseAbsDir dirStr of
    Just d -> pure d
    Nothing -> fail $ "unparseable temp dir: " <> dirStr
  createDirectoryIfMissing True (toFilePath dir)
  act dir `finally` removeDirectoryRecursive (toFilePath dir)

staticAnalysisSpecs :: Spec
staticAnalysisSpecs =
  describe "analyzeProjectStaticOnly (lockfile required)" $ do
    it "returns real results when a parsable Cargo.lock is present" $
      withTempDirIO $ \dir -> do
        let tomlFile = dir </> $(mkRelFile "Cargo.toml")
            lockFile = dir </> $(mkRelFile "Cargo.lock")
        writeFile (toFilePath tomlFile) demoManifest
        writeFile (toFilePath lockFile) demoLock
        let project = CargoProject dir tomlFile
        res <- runStaticAnalysis project
        case res of
          Failure ws eg -> expectationFailure $ "expected static analysis to succeed, got: " <> toString (renderFailureText ws eg)
          Success _ results -> do
            let graph = dependencyGraph results
            Graphing.vertexList graph `shouldContain` [demoDependency]
            dependencyManifestFiles results `shouldBe` [tomlFile]
    it "fatals with the missing-lockfile diagnostic when no Cargo.lock is present" $
      withTempDirIO $ \dir -> do
        let tomlFile = dir </> $(mkRelFile "Cargo.toml")
        writeFile (toFilePath tomlFile) demoManifest
        res <- runStaticAnalysis (CargoProject dir tomlFile)
        case res of
          Success _ _ -> expectationFailure "expected static analysis to fail without a Cargo.lock"
          Failure ws eg ->
            let rendered = renderFailureText ws eg
             in shouldContain (toString rendered) "Cannot analyze Cargo project statically: no Cargo.lock was found for cargo manifest"
    it "fatals with the unparseable-lockfile diagnostic when Cargo.lock cannot be parsed" $
      withTempDirIO $ \dir -> do
        let tomlFile = dir </> $(mkRelFile "Cargo.toml")
            lockFile = dir </> $(mkRelFile "Cargo.lock")
        writeFile (toFilePath tomlFile) demoManifest
        writeFile (toFilePath lockFile) corruptLock
        res <- runStaticAnalysis (CargoProject dir tomlFile)
        case res of
          Success _ _ -> expectationFailure "expected static analysis to fail for a corrupt Cargo.lock"
          Failure ws eg ->
            let rendered = renderFailureText ws eg
             in shouldContain (toString rendered) "Cannot analyze Cargo project statically: could not parse Cargo.lock for cargo manifest"

spec :: Spec
spec = do
  sourceDecisionSpecs
  fullAnalysisSpecs
  lockfileReadSpecs
  staticAnalysisSpecs
