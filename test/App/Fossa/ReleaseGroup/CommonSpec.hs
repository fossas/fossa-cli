module App.Fossa.ReleaseGroup.CommonSpec (
  spec,
) where

import App.Fossa.ReleaseGroup.Common (retrieveReleaseGroupId, retrieveReleaseGroupRelease)
import Fossa.API.CoreTypes (ReleaseGroupRelease (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  retrieveReleaseGroupIdSpec
  retrieveReleaseGroupReleaseSpec

retrieveReleaseGroupIdSpec :: Spec
retrieveReleaseGroupIdSpec = do
  describe "Delete Release Group" $ do
    it' "should fail when there are multiple release groups that match the input title" $ do
      let releaseGroups = [Fixtures.releaseGroup, Fixtures.releaseGroup]
      expectFatal' $ retrieveReleaseGroupId "example-title" releaseGroups
    it' "should return Nothing when release group title is not in the list of release groups" $ do
      let releaseGroups = [Fixtures.releaseGroup]
      res <- retrieveReleaseGroupId "non-existent" releaseGroups
      res `shouldBe'` Nothing
    it' "should return the release group id" $ do
      let releaseGroups = [Fixtures.releaseGroup]
      res <- retrieveReleaseGroupId "example-title" releaseGroups
      res `shouldBe'` Just 1

retrieveReleaseGroupReleaseSpec :: Spec
retrieveReleaseGroupReleaseSpec = do
  describe "Delete Release Group" $ do
    it' "should fail when there are multiple release groups that match the input release title" $ do
      let releases = [Fixtures.release, Fixtures.release]
      expectFatal' $ retrieveReleaseGroupRelease "example-release-title" releases
    it' "should fail when there are no releases that match the input release title" $ do
      let releases = [Fixtures.release]
      expectFatal' $ retrieveReleaseGroupRelease "non-existent" releases
    it' "should return the release group release id" $ do
      let releases = [Fixtures.release]
      res <- retrieveReleaseGroupRelease "example-release-title" releases
      releaseGroupReleaseId res `shouldBe'` 2
