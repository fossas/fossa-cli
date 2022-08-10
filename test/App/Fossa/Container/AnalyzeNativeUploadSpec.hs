module App.Fossa.Container.AnalyzeNativeUploadSpec (spec) where

import App.Fossa.Container.AnalyzeNative (uploadScan)
import App.Types (ProjectMetadata (..), ProjectRevision (..))
import Container.Types (ContainerScan (..), ContainerScanImage (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Fossa.API.Types (Organization (..), uploadLocator)
import Srclib.Types (Locator)
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (MockApi, alwaysReturns)

spec :: Spec
spec = do
  describe "Native Container Upload" $ do
    it' "should upload native container upload, when org supports native container scanning" $ do
      let org = Fixtures.organization{orgSupportsNativeContainerScan = True}
      GetOrganization `alwaysReturns` org
      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectUploadSuccess
      locator <- uploadScan fixtureRevision fixtureProjectMetadata fixtureContainerScan
      locator `shouldBe'` expectedLocator

    it' "should fail uploading native container scan, when org does not supports native container scanning" $ do
      let org = Fixtures.organization{orgSupportsNativeContainerScan = False}
      GetOrganization `alwaysReturns` org
      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectFatal' $ uploadScan fixtureRevision fixtureProjectMetadata fixtureContainerScan

fixtureProjectMetadata :: ProjectMetadata
fixtureProjectMetadata = ProjectMetadata Nothing Nothing Nothing Nothing Nothing Nothing Nothing

fixtureContainerScan :: ContainerScan
fixtureContainerScan = ContainerScan (ContainerScanImage "alpine" "3.1.4" []) "some-digest" "some-tag"

fixtureRevision :: ProjectRevision
fixtureRevision = ProjectRevision "some-tag" "some-digest" $ Just "master"

expectUploadSuccess :: Has MockApi sig m => m ()
expectUploadSuccess =
  UploadNativeContainerScan fixtureRevision fixtureProjectMetadata fixtureContainerScan
    `alwaysReturns` Fixtures.uploadResponse

expectedLocator :: Locator
expectedLocator = uploadLocator Fixtures.uploadResponse
