{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reachability.UploadSpec (spec) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.Analyze.Types (
  DiscoveredProjectIdentifier (..),
  DiscoveredProjectScan (..),
  SourceUnitReachabilityAttempt (..),
 )
import App.Fossa.Reachability.Types (
  CallGraphAnalysis (JarAnalysis),
  ContentRef (ContentRaw, ContentStoreKey),
  ParsedJar (..),
  ReachabilityConfig,
  SourceUnitReachability (
    callGraphAnalysis,
    srcUnitManifest,
    srcUnitName,
    srcUnitType
  ),
 )
import App.Fossa.Reachability.Upload (
  analyzeForReachability,
  callGraphOf,
  dependenciesOf,
  onlyFoundUnits,
  upload,
 )
import Control.Algebra (Has)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Effect.FossaApiClient (
  FossaApiClientF (..),
 )
import Data.ByteString.Lazy (ByteString)
import Data.Set qualified as Set
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Diag.Result (Result (Success))
import Graphing (empty)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  mkRelDir,
  mkRelFile,
  (</>),
 )
import Path.IO qualified as PIO
import Srclib.Types (
  Locator (..),
  SourceUnit (..),
 )
import Test.Effect (it', shouldBe')
import Test.Fixtures qualified as Fixture
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce)
import Types (DiscoveredProjectType (..), GraphBreadth (..))

spec :: Spec
spec = do
  dependenciesOfSpec
  callGraphOfSpec
  analyzeForReachabilitySpec
  uploadSpec

dependenciesOfSpec :: Spec
dependenciesOfSpec = describe "dependenciesOf" $
  it "should get dependencies" $ do
    let seen = Set.fromList $ dependenciesOf sourceUnit
    let expected =
          Set.fromList
            [ Locator "mvn" "com.github.seancfoley:ipaddress" (Just "5.4.0")
            , Locator "mvn" "com.github.spotbugs:spotbugs-annotations" (Just "4.8.3")
            , Locator "mvn" "org.apache.logging.log4j:log4j-core" (Just "2.22.1")
            ]
    expected `shouldBe` seen

run :: ReaderC ReachabilityConfig m a -> m a
run = runReader (mempty :: ReachabilityConfig)

callGraphOfSpec :: Spec
callGraphOfSpec = describe "callGraphOf" $ do
  it' "should return SkippedMissingDependencyAnalysis if project was skipped" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    let (dps, dpi) = skippedProject dir
    res <- run $ callGraphOf dps
    res `shouldBe'` (SourceUnitReachabilitySkippedMissingDependencyAnalysis dpi)

  it' "should return SkippedMissingDependencyAnalysis if project was skipped due to default filtering" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    let (dps, dpi) = skippedProjectByDefaultFilter dir
    res <- run $ callGraphOf dps
    res `shouldBe'` (SourceUnitReachabilitySkippedMissingDependencyAnalysis dpi)

  it' "should return SkippedPartialGraph if graph depth is partial" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    let (dps, dpi) = (mavenPartialScan dir)
    res <- run $ callGraphOf dps
    res `shouldBe'` SourceUnitReachabilitySkippedPartialGraph dpi

  it' "should return SkippedNotSupported for non-mvn or gradle project" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    let (dps, dpi) = (poetryCompleteScan dir)
    res <- run $ callGraphOf dps
    res `shouldBe'` (SourceUnitReachabilitySkippedNotSupported dpi)

analyzeForReachabilitySpec :: Spec
analyzeForReachabilitySpec =
  describe "analyzeForReachability" $
    it' "should return analyzed reachability unit" $ do
      dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
      analyzed <-
        run $
          analyzeForReachability
            [ fst $ skippedProject dir
            , fst $ skippedProjectByDefaultFilter dir
            , fst $ mavenPartialScan dir
            , fst $ poetryCompleteScan dir
            ]
      (onlyFoundUnits analyzed) `shouldBe'` []

uploadSpec :: Spec
uploadSpec = describe "dependenciesOf" $ do
  it' "should upload reachability units and content" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    file <- (</> sampleJar) <$> PIO.getCurrentDir

    let someContentKey = "someKey"
    let unit = mkReachabilityUnit dir [mkParsedJarRaw file Fixtures.sampleJarParsedContent']

    expectGetApiOpts
    expectReachabilityContentUpload Fixtures.sampleJarParsedContent' someContentKey

    let expectedUnit = mkReachabilityUnit dir [mkParsedJarS3 file someContentKey]
    expectReachabilityBuildUpload [expectedUnit]

    upload Fixtures.projectRevision Fixtures.projectMetadata [unit]

sourceUnit :: SourceUnit
sourceUnit = (head Fixture.sourceUnits){sourceUnitBuild = Just Fixture.sourceUnitBuildMaven}

sampleMavenProjectDir :: Path Rel Dir
sampleMavenProjectDir = $(mkRelDir "test/Reachability/testdata/maven-default/")

sampleJar :: Path Rel File
sampleJar = $(mkRelFile "test/Reachability/testdata/maven-default/target/project-1.0.0.jar")

skippedProject :: Path Abs Dir -> (DiscoveredProjectScan, DiscoveredProjectIdentifier)
skippedProject dir =
  ( SkippedDueToProvidedFilter dpi
  , dpi
  )
  where
    dpi :: DiscoveredProjectIdentifier
    dpi = DiscoveredProjectIdentifier dir MavenProjectType

skippedProjectByDefaultFilter :: Path Abs Dir -> (DiscoveredProjectScan, DiscoveredProjectIdentifier)
skippedProjectByDefaultFilter dir =
  ( SkippedDueToDefaultFilter dpi
  , dpi
  )
  where
    dpi :: DiscoveredProjectIdentifier
    dpi = DiscoveredProjectIdentifier dir MavenProjectType

mavenPartialScan :: Path Abs Dir -> (DiscoveredProjectScan, DiscoveredProjectIdentifier)
mavenPartialScan dir = mkDiscoveredProjectScan MavenProjectType dir Partial

poetryCompleteScan :: Path Abs Dir -> (DiscoveredProjectScan, DiscoveredProjectIdentifier)
poetryCompleteScan dir = mkDiscoveredProjectScan PoetryProjectType dir Complete

mkDiscoveredProjectScan :: DiscoveredProjectType -> Path Abs Dir -> GraphBreadth -> (DiscoveredProjectScan, DiscoveredProjectIdentifier)
mkDiscoveredProjectScan projectType dir breadth =
  ( Scanned
      dpi
      ( Success
          []
          (ProjectResult projectType dir empty breadth mempty)
      )
  , dpi
  )
  where
    dpi :: DiscoveredProjectIdentifier
    dpi = DiscoveredProjectIdentifier dir projectType

mkParsedJarRaw :: Path Abs File -> ByteString -> ParsedJar
mkParsedJarRaw file bs =
  ParsedJar
    { parsedJarPath = file
    , parsedJarContent = ContentRaw bs
    }

mkParsedJarS3 :: Path Abs File -> Text -> ParsedJar
mkParsedJarS3 file key =
  ParsedJar
    { parsedJarPath = file
    , parsedJarContent = ContentStoreKey key
    }

mkReachabilityUnit :: Path Abs Dir -> [ParsedJar] -> SourceUnitReachability
mkReachabilityUnit dir jars =
  Fixture.sourceUnitReachabilityNoAnalysis
    { srcUnitType = "maven"
    , srcUnitManifest = toText dir
    , srcUnitName = toText dir
    , callGraphAnalysis = JarAnalysis jars
    }

expectGetApiOpts :: (Has MockApi sig m) => m ()
expectGetApiOpts = GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectReachabilityContentUpload :: (Has MockApi sig m) => ByteString -> Text -> m ()
expectReachabilityContentUpload bs key = UploadContentForReachability bs `returnsOnce` key

expectReachabilityBuildUpload :: (Has MockApi sig m) => [SourceUnitReachability] -> m ()
expectReachabilityBuildUpload units =
  UploadBuildForReachability
    Fixtures.projectRevision
    Fixtures.projectMetadata
    units
    `returnsOnce` ()
