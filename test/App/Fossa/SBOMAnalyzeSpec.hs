module App.Fossa.SBOMAnalyzeSpec (spec) where

import App.Fossa.Config.SBOM.Analyze (JsonOutput (JsonOutput), SBOMAnalyzeConfig (..), SBOMAnalyzeOptions (..), cliParser)
import App.Fossa.Config.SBOM.Common (SBOMFile (..))
import App.Fossa.Config.Utils (parseArgString)
import App.Fossa.SBOM.Analyze (analyzeInternal)
import App.Types (BaseDir (..), ComponentUploadFileType (..), DependencyRebuild (DependencyRebuildReuseCache), ProjectRevision (ProjectRevision))
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Telemetry (withoutTelemetry)
import Control.Effect.FossaApiClient (FossaApiClientF (..), PackageRevision (..))
import Data.Flag (fromFlag, toFlag')
import Effect.Logger (Severity (SevInfo))
import Fossa.API.Types (Archive (..))
import Path.IO (getCurrentDir)
import Test.Effect (it', shouldBe')
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
      let config = SBOMAnalyzeConfig (BaseDir currDir) Fixtures.apiOpts (SBOMFile "test/App/Fossa/SBOM/testdata/sampleCycloneDX.json") DependencyRebuildReuseCache Nothing (toFlag' False) revision Nothing SevInfo

      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectOrganization
      expectGetSignedUrl PackageRevision{packageName = "somesbom", packageVersion = "1.2.3"}
      expectUploadArchive
      expectQueueSBOMBuild archive

      ignoreDebug . withoutTelemetry . runMockApi $ analyzeInternal config

  describe "SBOM Analyze cliParser" $ do
    it' "should default --json off" $ do
      opts <- parseArgString cliParser "test/App/Fossa/SBOM/testdata/sampleCycloneDX.json"
      fromFlag JsonOutput (jsonOutput opts) `shouldBe'` False

    it' "should parse --json into the options" $ do
      opts <- parseArgString cliParser "--json test/App/Fossa/SBOM/testdata/sampleCycloneDX.json"
      fromFlag JsonOutput (jsonOutput opts) `shouldBe'` True

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
