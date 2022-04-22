{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.LicenseScannerSpec (spec) where

import Analysis.FixtureUtils (
  FixtureArtifact (FixtureArtifact),
  getArtifact,
 )
import App.Fossa.ArchiveUploader (VendoredDependency (VendoredDependency))
import App.Fossa.LicenseScanner (scanVendoredDep)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (runStickyLogger)
import Diag.Result (Result (Failure, Success), renderFailure)
import Effect.Exec (runExecIO)
import Effect.Logger (Severity (SevDebug), withDefaultLogger)
import Effect.ReadFS (runReadFSIO)
import Path (reldir, (</>))
import Path.IO qualified as PIO
import Test.Hspec (Spec, it, shouldBe)

recursiveArchive :: FixtureArtifact
recursiveArchive =
  FixtureArtifact
    "https://github.com/spatten/cli-license-scan-integration-test-fixtures/archive/refs/heads/main.zip"
    [reldir|manual-deps/license-scanner/|]
    [reldir|recursive-archive|]

vendoredDep :: VendoredDependency
vendoredDep =
  VendoredDependency
    "recursive-archive-test"
    "vendor/foo.tar.gz"
    (Just "0.0.1")

spec :: Spec
spec = do
  it "should find licenses" $ do
    extractedDir <- getArtifact recursiveArchive
    let scanDir = extractedDir </> [reldir|cli-license-scan-integration-test-fixtures-main/recursive-archive|]
    units <- runStack $ runDiagnostics $ withDefaultLogger SevDebug $ runStickyLogger SevDebug $ runExecIO $ runReadFSIO $ scanVendoredDep scanDir vendoredDep
    -- PIO.removeDirRecur extractedDir
    case units of
      Failure ws eg -> fail (show (renderFailure ws eg "An issue occurred"))
      Success _ us -> (length us) `shouldBe` 2
