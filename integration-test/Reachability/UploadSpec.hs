{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reachability.UploadSpec (spec) where

import Analysis.FixtureUtils (FixtureEnvironment (..), TestC, testRunnerWithLogger, withResult)
import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.Analyze.Types (
  AnalysisScanResult (..),
  DiscoveredProjectIdentifier (..),
  DiscoveredProjectScan (..),
 )
import App.Fossa.Reachability.Jar (callGraphFromJar)
import App.Fossa.Reachability.Types (
  CallGraphAnalysis (..),
  ContentRef (..),
  ParsedJar (..),
  SourceUnitReachability (..),
 )
import App.Fossa.Reachability.Upload (
  analyzeForReachability,
  callGraphOf,
 )
import Data.ByteString.Lazy qualified as LB
import Data.Foldable (for_)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text.Encoding qualified as TL
import Diag.Result (Result (..))
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
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Text.RawString.QQ (r)
import Types (
  DiscoveredProjectType (MavenProjectType),
  GraphBreadth (..),
 )

java8 :: FixtureEnvironment
java8 = NixEnv ["jdk8"]

java11 :: FixtureEnvironment
java11 = NixEnv ["jdk11"]

java17 :: FixtureEnvironment
java17 = NixEnv ["jdk17"]

java21 :: FixtureEnvironment
java21 = NixEnv ["jdk21"]

run :: FixtureEnvironment -> TestC IO a -> IO (Result a)
run env act = testRunnerWithLogger act env

spec :: Spec
spec = describe "Reachability" $ do
  describe "callGraphFromJar" $ do
    jarFile <- runIO sampleJarFile
    for_
      [ java8
      , java11
      , java17
      , java21
      ]
      $ \env -> do
        it ("should compute in java: " <> show env) $ do
          let expected = Just (sampleJarParsed jarFile)
          res <- run env $ callGraphFromJar jarFile

          withResult res $ \_ res' -> res' `shouldBe` expected

  describe "callGraphOf" $ do
    projDir <- (</> sampleMavenProjectDir) <$> runIO PIO.getCurrentDir
    jarFile <- (</> sampleMavenProjectJar) <$> runIO PIO.getCurrentDir

    it "should retrieve call graph" $ do
      let expected = Just (mavenCompleteScanUnit projDir jarFile)
      resp <- run java8 $ callGraphOf (mavenCompleteScan projDir)
      withResult resp $ \_ res -> res `shouldBe` expected

  describe "analyzeForReachability" $ do
    projDir <- (</> sampleMavenProjectDir) <$> runIO PIO.getCurrentDir
    jarFile <- (</> sampleMavenProjectJar) <$> runIO PIO.getCurrentDir

    it "should return analyzed reachability unit" $ do
      let expected = [mavenCompleteScanUnit projDir jarFile]
      let analysisResult =
            AnalysisScanResult
              [mavenCompleteScan projDir]
              successNothing
              successNothing
              successNothing
              successNothing
              successNothing

      analyzed <- run java8 $ analyzeForReachability analysisResult
      withResult analyzed $ \_ analyzed' -> analyzed' `shouldBe` expected

sampleMavenProjectDir :: Path Rel Dir
sampleMavenProjectDir = $(mkRelDir "test/Reachability/testdata/maven-default/")

sampleMavenProjectJar :: Path Rel File
sampleMavenProjectJar = $(mkRelFile "test/Reachability/testdata/maven-default/target/project-1.0.0.jar")

mavenCompleteScan :: Path Abs Dir -> DiscoveredProjectScan
mavenCompleteScan dir = mkDiscoveredProjectScan MavenProjectType dir Complete

mavenCompleteScanUnit :: Path Abs Dir -> Path Abs File -> SourceUnitReachability
mavenCompleteScanUnit projDir jarFile =
  mkReachabilityUnit
    projDir
    [ ParsedJar
        jarFile
        (ContentRaw sampleJarParsedContent')
    ]

mkDiscoveredProjectScan :: DiscoveredProjectType -> Path Abs Dir -> GraphBreadth -> DiscoveredProjectScan
mkDiscoveredProjectScan projectType dir breadth =
  Scanned
    (DiscoveredProjectIdentifier dir projectType)
    ( Success
        []
        (ProjectResult projectType dir empty breadth mempty)
    )

sampleJarFile :: IO (Path Abs File)
sampleJarFile = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/sample.jar"))

successNothing :: Result (Maybe a)
successNothing = Success [] Nothing

mkReachabilityUnit :: Path Abs Dir -> [ParsedJar] -> SourceUnitReachability
mkReachabilityUnit dir jars =
  SourceUnitReachability
    "maven"
    (toText dir)
    (toText dir)
    []
    []
    (JarAnalysis jars)

sampleJarParsed :: Path Abs File -> ParsedJar
sampleJarParsed path =
  ParsedJar
    { parsedJarPath = path
    , parsedJarContent = ContentRaw sampleJarParsedContent'
    }

sampleJarParsedContent :: Text
sampleJarParsedContent =
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

sampleJarParsedContent' :: LB.ByteString
sampleJarParsedContent' = LB.fromStrict . TL.encodeUtf8 $ sampleJarParsedContent
