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
import Data.ByteString.Lazy qualified as LB
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Data.Text.Encoding qualified as TL
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
import Text.RawString.QQ (r)
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
callGraphOfSpec = describe "c,allGraphOf" $ do
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

  it' "should return reachability unit for mvn project" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    file <- (</> sampleJar) <$> PIO.getCurrentDir
    res <- callGraphOf (mavenCompleteScan dir)

    let unit = mkReachabilityUnit dir [mkParsedJarRaw file sampleJarContent']
    res `shouldBe'` Just unit

analyzeForReachabilitySpec :: Spec
analyzeForReachabilitySpec =
  describe "analyzeForReachability" $
    it' "should return analyzed reachability unit" $ do
      dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
      file <- (</> sampleJar) <$> PIO.getCurrentDir
      let unit = mkReachabilityUnit dir [mkParsedJarRaw file sampleJarContent']
      let analysisResult =
            mkAnalysisResult
              [ skippedProject dir
              , skippedProjectByDefaultFilter dir
              , mavenPartialScan dir
              , poetryCompleteScan dir
              , mavenCompleteScan dir
              ]

      analyzed <- analyzeForReachability analysisResult
      analyzed `shouldBe'` [unit]

uploadSpec :: Spec
uploadSpec = describe "dependenciesOf" $ do
  it' "should upload reachability units and content" $ do
    dir <- (</> sampleMavenProjectDir) <$> PIO.getCurrentDir
    file <- (</> sampleJar) <$> PIO.getCurrentDir

    let someContentKey = "someKey"
    let unit = mkReachabilityUnit dir [mkParsedJarRaw file sampleJarContent']

    expectGetApiOpts
    expectReachabilityContentUpload sampleJarContent' someContentKey

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

mavenCompleteScan :: Path Abs Dir -> DiscoveredProjectScan
mavenCompleteScan dir = mkDiscoveredProjectScan MavenProjectType dir Complete

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

sampleJarContent :: Text
sampleJarContent =
  [r|C:vuln.project.sample.App java.lang.Object
C:vuln.project.sample.App java.net.URI
C:vuln.project.sample.App java.lang.System
C:vuln.project.sample.App vuln.project.sample.App
C:vuln.project.sample.App java.io.PrintStream
C:vuln.project.sample.App org.dom4j.io.SAXReader
C:vuln.project.sample.App java.lang.Exception
C:vuln.project.sample.App org.dom4j.DocumentException
M:vuln.project.sample.App:<init>() (O)java.lang.Object:<init>()
M:vuln.project.sample.App:main(java.lang.String[]) (O)java.net.URI:<init>(java.lang.String)
M:vuln.project.sample.App:main(java.lang.String[]) (M)java.net.URI:toURL()
M:vuln.project.sample.App:main(java.lang.String[]) (S)vuln.project.sample.App:parse(java.net.URL)
M:vuln.project.sample.App:main(java.lang.String[]) (M)java.io.PrintStream:println(java.lang.Object)
M:vuln.project.sample.App:parse(java.net.URL) (O)org.dom4j.io.SAXReader:<init>()
M:vuln.project.sample.App:parse(java.net.URL) (M)org.dom4j.io.SAXReader:read(java.net.URL)|]

sampleJarContent' :: ByteString
sampleJarContent' = LB.fromStrict . TL.encodeUtf8 $ sampleJarContent

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
