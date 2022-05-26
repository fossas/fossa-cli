{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.ArchiveUploaderSpec (spec) where

import App.Fossa.ArchiveUploader (archiveUploadSourceUnit)
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..), PackageRevision (..))
import Fossa.API.Types (Archive (..))
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Test.Effect (it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, runIO)
import Test.MockApi (MockApi, returnsOnce, returnsOnceForAnyRequest)
import Test.MockApiExpectations (expectGetApiOpts, expectGetOrganization, expectGetSignedUrl)

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/LicenseScanner/testdata/repo")

spec :: Spec
spec = do
  describe "archiveUploadSourceUnit" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir
    it' "should do the archive upload workflow" $ do
      expectGetApiOpts
      expectGetOrganization
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadArchive -- Fixtures.firstArchive
      expectQueueArchiveBuild Fixtures.firstArchive
      locators <- archiveUploadSourceUnit scanDir Fixtures.vendoredDeps
      locators `shouldBe'` Fixtures.locators

expectUploadArchive :: Has MockApi sig m => m ()
expectUploadArchive = do
  UploadArchive Fixtures.signedUrl "some/path" `returnsOnceForAnyRequest` "success"

expectQueueArchiveBuild :: Has MockApi sig m => Archive -> m ()
expectQueueArchiveBuild archive =
  QueueArchiveBuild archive `returnsOnce` pure "success"
