module App.Fossa.SBOMAnalyzeSpec (spec) where

import App.Fossa.Config.SBOM.Analyze (SBOMAnalyzeConfig (..))
import App.Fossa.Config.SBOM.Common (SBOMFile (..))
import App.Fossa.SBOM.Analyze (analyzeInternal)
import App.Types (BaseDir (..), ComponentUploadFileType (..), DependencyRebuild (DependencyRebuildReuseCache), ProjectRevision (ProjectRevision))
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Telemetry (withoutTelemetry)
import Control.Effect.FossaApiClient (FossaApiClientF (..), PackageRevision (..))
import Effect.Logger (Severity (SevInfo))
import Fossa.API.Types (Archive (..))
import Path.IO (getCurrentDir)
import Test.Effect (it')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, runIO)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce, returnsOnceForAnyRequest, runMockApi)

spec :: Spec
spec = do
  describe "SBOM Analyze" $ do
    currDir <- runIO getCurrentDir
    it' "should upload a file" $ do
      let archive = Archive "somesbom" "1.2.3" Nothing Nothing
      let revision = ProjectRevision "somesbom" "1.2.3" Nothing
      let config = SBOMAnalyzeConfig (BaseDir currDir) Fixtures.apiOpts (SBOMFile "test/App/Fossa/SBOM/testdata/sampleCycloneDX.json") SevInfo DependencyRebuildReuseCache Nothing revision

      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectOrganization
      expectGetSignedUrl PackageRevision{packageName = "somesbom", packageVersion = "1.2.3"}
      expectUploadArchive
      expectQueueSBOMBuild archive

      ignoreDebug . withoutTelemetry . runMockApi $ analyzeInternal config

expectOrganization :: Has MockApi sig m => m ()
expectOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectUploadArchive :: Has MockApi sig m => m ()
expectUploadArchive = do
  UploadArchive Fixtures.signedUrl "test/App/Fossa/SBOM/testdata/sampleCycloneDX.json" `returnsOnceForAnyRequest` "success"

expectQueueSBOMBuild :: Has MockApi sig m => Archive -> m ()
expectQueueSBOMBuild archive =
  QueueSBOMBuild archive Nothing DependencyRebuildReuseCache `returnsOnce` ()

expectGetSignedUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedUrl packageRevision = GetSignedUploadUrl SBOMUpload packageRevision `alwaysReturns` Fixtures.signedUrl
