{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.BinaryDeps.NupkgSpec (spec) where

import App.Fossa.BinaryDeps.Nupkg (resolveNupkg)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import DepTypes (DepType (NuGetType))
import Diag.Result (Result (Failure, Success))
import Effect.Logger (Severity (SevError), withDefaultLogger)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, Dir, File, Path, mkRelDir, mkRelFile, (</>))
import Path.Extra (tryMakeRelative)
import Path.IO qualified as PIO
import Srclib.Types (
  BinaryDiscoveredDep (..),
  SourceUserDefDep (..),
 )
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "handle nupkg with no license tag (legacy licenseUrl only)" $ do
    root <- runIO testdataParentDir
    target <- runIO withoutLicenseTag
    result <- runIO . runStack . withDefaultLogger SevError . runDiagnostics . runReadFSIO $ resolveNupkg root target

    it "parses the nupkg correctly" $ case result of
      Failure _ _ -> expectationFailure "could not parse nupkg"
      Success _ dep -> dep `shouldBe` Just (expectedWithoutLicenseTag root)

  describe "handle nupkg with a license expression tag" $ do
    root <- runIO testdataParentDir
    target <- runIO withLicenseExpression
    result <- runIO . runStack . withDefaultLogger SevError . runDiagnostics . runReadFSIO $ resolveNupkg root target

    it "parses the nupkg correctly" $ case result of
      Failure _ _ -> expectationFailure "could not parse nupkg"
      Success _ dep -> dep `shouldBe` Just (expectedWithLicenseExpression root)

testdataParentDir :: IO (Path Abs Dir)
testdataParentDir = PIO.resolveDir' "test/App/Fossa/BinaryDeps"

withoutLicenseTag :: IO (Path Abs File)
withoutLicenseTag = PIO.resolveFile' "test/App/Fossa/BinaryDeps/testdata/jquery.3.4.1.nupkg"

withLicenseExpression :: IO (Path Abs File)
withLicenseExpression = PIO.resolveFile' "test/App/Fossa/BinaryDeps/testdata/nuget.versioning.6.9.1.nupkg"

expectedWithoutLicenseTag :: Path Abs Dir -> BinaryDiscoveredDep
expectedWithoutLicenseTag root = do
  let path = root </> $(mkRelDir "testdata") </> $(mkRelFile "jquery.3.4.1.nupkg")
  let rel = tryMakeRelative root path
  LocatorDep
    ( NuGetType
    , SourceUserDefDep
        "jQuery"
        "3.4.1"
        ""
        (Just "jQuery")
        Nothing
        (Just rel)
    )

expectedWithLicenseExpression :: Path Abs Dir -> BinaryDiscoveredDep
expectedWithLicenseExpression root = do
  let path = root </> $(mkRelDir "testdata") </> $(mkRelFile "nuget.versioning.6.9.1.nupkg")
  let rel = tryMakeRelative root path
  LocatorDep
    ( NuGetType
    , SourceUserDefDep
        "NuGet.Versioning"
        "6.9.1"
        "Apache-2.0"
        (Just "NuGet.Versioning")
        Nothing
        (Just rel)
    )
