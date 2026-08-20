{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.BinaryDeps.WhlSpec (spec) where

import App.Fossa.BinaryDeps.Whl (resolveWhl)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Data.String.Conversion (toText)
import DepTypes (DepType (PipType))
import Diag.Result (Result (Failure, Success))
import Effect.Logger (Severity (SevError), withDefaultLogger)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, Dir, File, Path, mkRelDir, mkRelFile, (</>))
import Path.Extra (tryMakeRelative)
import Path.IO qualified as PIO
import Srclib.Types
  ( BinaryDiscoveredDep (..),
    SourceUserDefDep (..),
  )
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "handle whl with manifest version older than 2.4" $ do
    root <- runIO testdataParentDir
    target <- runIO withOlderThanTwoFour
    result <- runIO . runStack . withDefaultLogger SevError . runDiagnostics . runReadFSIO $ resolveWhl root target

    it "parses the whl correctly" $ case result of
      Failure _ _ -> expectationFailure "could not parse whl"
      Success _ dep -> dep `shouldBe` Just (expectedOlderThanTwoFour root)

  describe "handle whl with manifest version of 2.4" $ do
    root <- runIO testdataParentDir
    target <- runIO withEqualToTwoFour
    result <- runIO . runStack . withDefaultLogger SevError . runDiagnostics . runReadFSIO $ resolveWhl root target

    it "parses the whl correctly" $ case result of
      Failure _ _ -> expectationFailure "could not parse whl"
      Success _ dep -> dep `shouldBe` Just (expectedEqualToTwoFour root)

testdataParentDir :: IO (Path Abs Dir)
testdataParentDir = PIO.resolveDir' "test/App/Fossa/BinaryDeps"

withOlderThanTwoFour :: IO (Path Abs File)
withOlderThanTwoFour = PIO.resolveFile' "test/App/Fossa/BinaryDeps/testdata/a4-0.2.7-py3-none-any.whl"

withEqualToTwoFour :: IO (Path Abs File)
withEqualToTwoFour = PIO.resolveFile' "test/App/Fossa/BinaryDeps/testdata/markupsafe-3.0.3-cp314-cp314t-win_arm64.whl"

expectedOlderThanTwoFour :: Path Abs Dir -> BinaryDiscoveredDep
expectedOlderThanTwoFour root = do
  let path = root </> $(mkRelDir "testdata") </> $(mkRelFile "a4-0.2.7-py3-none-any.whl")
  let rel = tryMakeRelative root path
  LocatorDep
    ( PipType,
      SourceUserDefDep
        (toText rel)
        "1.0"
        ""
        (Just "org.jruby:yecht")
        Nothing
        (Just rel)
    )

expectedEqualToTwoFour :: Path Abs Dir -> BinaryDiscoveredDep
expectedEqualToTwoFour root = do
  let path = root </> $(mkRelDir "testdata") </> $(mkRelFile "markupsafe-3.0.3-cp314-cp314t-win_arm64.whl")
  let rel = tryMakeRelative root path
  LocatorDep
    ( PipType,
      SourceUserDefDep
        (toText rel)
        "1.5.4"
        ""
        (Just "io.micrometer#micrometer-registry-prometheus;1.5.4")
        Nothing
        (Just rel)
    )
