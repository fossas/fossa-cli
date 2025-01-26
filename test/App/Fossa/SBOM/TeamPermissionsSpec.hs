module App.Fossa.SBOM.TeamPermissionsSpec (spec) where

import App.Fossa.Config.SBOM.Analyze (SBOMAnalyzeConfig (..))
import App.Fossa.Config.SBOM.Common (SBOMFile (..))
import App.Fossa.SBOM.Analyze (analyze)
import App.Types (BaseDir (..), ComponentUploadFileType (..), DependencyRebuild (DependencyRebuildReuseCache), ProjectRevision (ProjectRevision), ProjectMetadata (..))
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Telemetry (withoutTelemetry)
import Control.Effect.FossaApiClient (FossaApiClientF (..), PackageRevision (..))
import Effect.Logger (Severity (SevInfo))
import Fossa.API.Types (Archive (..), CustomBuildUploadPermissions (..), ProjectPermissionStatus (..))
import Path.IO (getCurrentDir)
import Test.Effect (it')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, runIO)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce, returnsOnceForAnyRequest, runMockApi)
import Data.Text (Text)

spec :: Spec
spec = do
  describe "SBOM Team Permissions" $ do
    currDir <- runIO getCurrentDir
    let teamName = "Team RL" :: Text
    
    it' "should succeed with team-scoped permissions" $ do
      let archive = Archive "somesbom" "1.2.3" Nothing Nothing
      let revision = ProjectRevision "somesbom" "1.2.3" Nothing
      let config = SBOMAnalyzeConfig 
            { sbomBaseDir = BaseDir currDir
            , sbomApiOpts = Fixtures.apiOpts
            , sbomPath = SBOMFile "test/App/Fossa/SBOM/testdata/sampleCycloneDX.json"
            , severity = SevInfo
            , sbomRebuild = DependencyRebuildReuseCache
            , sbomTeam = Just teamName
            , sbomRevision = revision
            }

      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectOrganization
      expectTeamPermissions teamName
      expectGetSignedUrl PackageRevision{packageName = "somesbom", packageVersion = "1.2.3"}
      expectUploadArchive
      expectQueueSBOMBuild archive (Just teamName)

      ignoreDebug . withoutTelemetry . runMockApi $ analyze config

expectOrganization :: Has MockApi sig m => m ()
expectOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectTeamPermissions :: Has MockApi sig m => Text -> m ()
expectTeamPermissions teamName = 
  GetCustomBuildPermissions (ProjectRevision "somesbom" "1.2.3" Nothing) (ProjectMetadata Nothing Nothing Nothing Nothing (Just teamName) Nothing [] Nothing)
    `alwaysReturns` CustomBuildUploadPermissions
      { projectPermissionStatus = InvalidCreateProjectOnlyToTeamPermission
      , maybeReleaseGroupPermissionStatus = Nothing
      }

expectUploadArchive :: Has MockApi sig m => m ()
expectUploadArchive = do
  UploadArchive Fixtures.signedUrl "test/App/Fossa/SBOM/testdata/sampleCycloneDX.json" `returnsOnceForAnyRequest` "success"

expectQueueSBOMBuild :: Has MockApi sig m => Archive -> Maybe Text -> m ()
expectQueueSBOMBuild archive team =
  QueueSBOMBuild archive team DependencyRebuildReuseCache `returnsOnce` ()

expectGetSignedUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedUrl packageRevision = 
  GetSignedUploadUrl SBOMUpload packageRevision `alwaysReturns` Fixtures.signedUrl
