{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.BinaryDeps.JarSpec (spec) where

import App.Fossa.BinaryDeps.Jar (resolveJar)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Data.String.Conversion (toText)
import Diag.Result (Result (Failure, Success))
import Effect.Logger (Severity (SevError), withDefaultLogger)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, Dir, File, Path, mkRelDir, mkRelFile, (</>))
import Path.Extra (tryMakeRelative)
import Path.IO qualified as PIO
import Srclib.Types (SourceUserDefDep (..))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "handle JAR with multiple pom.xml" $ do
    root <- runIO testdataParentDir
    target <- runIO withMultiplePoms
    result <- runIO . runStack . withDefaultLogger SevError . runDiagnostics . runReadFSIO $ resolveJar root target

    it "parses the jar correctly" $ case result of
      Failure _ _ -> expectationFailure "could not parse jar"
      Success _ dep -> dep `shouldBe` Just (expectedMultiplePoms root)

  describe "handle JAR with one pom.xml" $ do
    root <- runIO testdataParentDir
    target <- runIO withLicenseInPom
    result <- runIO . runStack . withDefaultLogger SevError . runDiagnostics . runReadFSIO $ resolveJar Nothing root target

    it "parses the jar correctly" $ case result of
      Failure _ _ -> expectationFailure "could not parse jar"
      Success _ dep -> dep `shouldBe` Just (expectedLicenseInPom root)

  describe "handle JAR without pom.xml" $ do
    root <- runIO testdataParentDir
    target <- runIO withMetaInfManifest
    result <- runIO . runStack . withDefaultLogger SevError . runDiagnostics . runReadFSIO $ resolveJar Nothing root target

    it "parses the jar correctly" $ case result of
      Failure _ _ -> expectationFailure "could not parse jar"
      Success _ dep -> dep `shouldBe` Just (expectedMetaInfManifest root)

testdataParentDir :: IO (Path Abs Dir)
testdataParentDir = PIO.resolveDir' "test/App/Fossa/BinaryDeps"

withMultiplePoms :: IO (Path Abs File)
withMultiplePoms = PIO.resolveFile' "test/App/Fossa/BinaryDeps/testdata/jruby-complete-1.7.12.jar"

withLicenseInPom :: IO (Path Abs File)
withLicenseInPom = PIO.resolveFile' "test/App/Fossa/BinaryDeps/testdata/json-simple-1.1.1.7.jar"

withMetaInfManifest :: IO (Path Abs File)
withMetaInfManifest = PIO.resolveFile' "test/App/Fossa/BinaryDeps/testdata/micrometer-registry-prometheus-1.5.4.jar"

expectedMultiplePoms :: Path Abs Dir -> SourceUserDefDep
expectedMultiplePoms root = do
  let path = root </> $(mkRelDir "testdata") </> $(mkRelFile "jruby-complete-1.7.12.jar")
  let rel = tryMakeRelative root path
  SourceUserDefDep (toText rel) "1.0" "" (Just "org.jruby:yecht") Nothing (Just rel)

expectedLicenseInPom :: Path Abs Dir -> SourceUserDefDep
expectedLicenseInPom root = do
  let path = root </> $(mkRelDir "testdata") </> $(mkRelFile "json-simple-1.1.1.7.jar")
  let rel = tryMakeRelative root path
  SourceUserDefDep (toText rel) "1.1.1" "The Apache Software License, Version 2.0" (Just "com.googlecode.json-simple:json-simple") Nothing (Just rel)

expectedMetaInfManifest :: Path Abs Dir -> SourceUserDefDep
expectedMetaInfManifest root = do
  let path = root </> $(mkRelDir "testdata") </> $(mkRelFile "micrometer-registry-prometheus-1.5.4.jar")
  let rel = tryMakeRelative root path
  SourceUserDefDep (toText rel) "1.5.4" "" (Just "io.micrometer#micrometer-registry-prometheus;1.5.4") Nothing (Just rel)
