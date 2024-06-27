module App.Fossa.Container.AnalyzeNativeUploadSpec (spec) where

import App.Fossa.Config.Container.Analyze (JsonOutput (..))
import App.Fossa.Container.AnalyzeNative (uploadScan)
import App.Types (ProjectMetadata (..), ProjectRevision (..))
import Container.Types (ContainerScan (..), ContainerScanImage (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Data.Flag (Flag, toFlag)
import Fossa.API.Types (OrgId (OrgId), Organization (..), Subscription (Premium), uploadLocator)
import Srclib.Types (Locator)
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns)
import Types (ArchiveUploadType (CLILicenseScan))

spec :: Spec
spec = do
  describe "Native Container Upload" $ do
    it' "should upload native container upload, when org supports native container scanning" $ do
      let org = Fixtures.organization{orgSupportsNativeContainerScan = True}
      GetOrganization `alwaysReturns` org
      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectUploadSuccess
      locator <- uploadScan org fixtureRevision fixtureProjectMetadata (fixtureJsonOutput False) fixtureContainerScan
      locator `shouldBe'` expectedLocator

    it' "should fail uploading native container scan, when org does not supports native container scanning" $ do
      let org = Fixtures.organization{orgSupportsNativeContainerScan = False}
      GetOrganization `alwaysReturns` org
      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectFatal' $ uploadScan org fixtureRevision fixtureProjectMetadata (fixtureJsonOutput False) fixtureContainerScan

    -- As with the version of this test for App.Fossa.Analyze.UploadSpec, this
    -- is just checking it doesn't fail.
    it' "should render JSON when requested" $ do
      let org = Fixtures.organization{orgSupportsNativeContainerScan = True}
      GetOrganization `alwaysReturns` org
      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectUploadSuccess
      locator <- uploadScan org fixtureRevision fixtureProjectMetadata (fixtureJsonOutput True) fixtureContainerScan
      locator `shouldBe'` expectedLocator

fixtureProjectMetadata :: ProjectMetadata
fixtureProjectMetadata = ProjectMetadata Nothing Nothing Nothing Nothing Nothing Nothing ["label-1", "label-2"] Nothing

fixtureContainerScan :: ContainerScan
fixtureContainerScan = ContainerScan (ContainerScanImage "alpine" "3.1.4" []) "some-digest" "some-tag"

fixtureRevision :: ProjectRevision
fixtureRevision = ProjectRevision "some-tag" "some-digest" $ Just "master"

fixtureJsonOutput :: Bool -> Flag JsonOutput
fixtureJsonOutput = toFlag JsonOutput

expectUploadSuccess :: (Has MockApi sig m) => m ()
expectUploadSuccess =
  UploadNativeContainerScan fixtureRevision fixtureProjectMetadata fixtureContainerScan
    `alwaysReturns` Fixtures.uploadResponse

expectedLocator :: Locator
expectedLocator = uploadLocator Fixtures.uploadResponse
