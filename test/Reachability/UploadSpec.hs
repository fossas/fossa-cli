{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reachability.UploadSpec (spec) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.Analyze.Types (AnalysisScanResult (..), DiscoveredProjectIdentifier (..), DiscoveredProjectScan (..))
import App.Fossa.Reachability.Types (
  CallGraphAnalysis (JarAnalysis),
  ContentRef (ContentRaw, ContentStoreKey),
  ParsedJar (..),
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
  upload,
 )
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (
  FossaApiClientF (..),
 )
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty qualified as NE
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

callGraphOfSpec :: Spec
callGraphOfSpec = describe "callGraphOf" $ do
  it' "should not return reachability unit if project was skipped" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    res <- callGraphOf (skippedProject dir)
    res `shouldBe'` Nothing

  it' "should not return reachability unit if project was skipped due to default filtering" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    res <- callGraphOf (skippedProjectByDefaultFilter dir)
    res `shouldBe'` Nothing

  it' "should not return reachability unit if graph depth is partial" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    res <- callGraphOf (mavenPartialScan dir)
    res `shouldBe'` Nothing

  it' "should not reachability unit for non-mvn project" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    res <- callGraphOf (poetryCompleteScan dir)
    res `shouldBe'` Nothing

analyzeForReachabilitySpec :: Spec
analyzeForReachabilitySpec =
  describe "analyzeForReachability" $
    it' "should return analyzed reachability unit" $ do
      dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
      let analysisResult =
            mkAnalysisResult
              [ skippedProject dir
              , skippedProjectByDefaultFilter dir
              , mavenPartialScan dir
              , poetryCompleteScan dir
              ]

      analyzed <- analyzeForReachability analysisResult
      analyzed `shouldBe'` []

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
sourceUnit = (NE.head Fixture.sourceUnits){sourceUnitBuild = Just Fixture.sourceUnitBuildMaven}

sampleMavenProjectDir :: Path Rel Dir
sampleMavenProjectDir = $(mkRelDir "test/Reachability/testdata/maven-default/")

sampleJar :: Path Rel File
sampleJar = $(mkRelFile "test/Reachability/testdata/maven-default/target/project-1.0.0.jar")

skippedProject :: Path Abs Dir -> DiscoveredProjectScan
skippedProject dir =
  SkippedDueToProvidedFilter
    (DiscoveredProjectIdentifier dir MavenProjectType)

skippedProjectByDefaultFilter :: Path Abs Dir -> DiscoveredProjectScan
skippedProjectByDefaultFilter dir =
  SkippedDueToDefaultProductionFilter
    (DiscoveredProjectIdentifier dir MavenProjectType)

mavenPartialScan :: Path Abs Dir -> DiscoveredProjectScan
mavenPartialScan dir = mkDiscoveredProjectScan MavenProjectType dir Partial

poetryCompleteScan :: Path Abs Dir -> DiscoveredProjectScan
poetryCompleteScan dir = mkDiscoveredProjectScan PoetryProjectType dir Complete

mkDiscoveredProjectScan :: DiscoveredProjectType -> Path Abs Dir -> GraphBreadth -> DiscoveredProjectScan
mkDiscoveredProjectScan projectType dir breadth =
  Scanned
    (DiscoveredProjectIdentifier dir projectType)
    ( Success
        []
        (ProjectResult projectType dir empty breadth mempty)
    )

mkAnalysisResult :: [DiscoveredProjectScan] -> AnalysisScanResult
mkAnalysisResult dps =
  AnalysisScanResult
    dps
    (Success [] Nothing)
    (Success [] Nothing)
    (Success [] Nothing)
    (Success [] Nothing)
    (Success [] Nothing)

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
