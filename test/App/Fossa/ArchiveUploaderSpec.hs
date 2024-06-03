{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.ArchiveUploaderSpec (spec) where

import App.Fossa.ArchiveUploader (archiveUploadSourceUnit)
import App.Types (ComponentUploadFileType (..), DependencyRebuild (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..), PackageRevision (..))
import Data.List.NonEmpty qualified as NE
import Fossa.API.Types (Archive (..))
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Test.Effect (it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, runIO)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce, returnsOnceForAnyRequest)

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/VendoredDependency/testdata/repo")

spec :: Spec
spec = do
  describe "archiveUploadSourceUnit" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir
    it' "should do the archive upload workflow for a single archive" $ do
      GetOrganization `alwaysReturns` Fixtures.organization
      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadArchive
      expectQueueArchiveBuild Fixtures.firstArchive
      locators <- archiveUploadSourceUnit DependencyRebuildReuseCache scanDir $ Fixtures.firstVendoredDep NE.:| []
      locators `shouldBe'` (Fixtures.firstLocator NE.:| [])

    it' "should do the archive upload workflow for multiple archives" $ do
      GetOrganization `alwaysReturns` Fixtures.organization
      GetApiOpts `alwaysReturns` Fixtures.apiOpts
      expectGetSignedUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectGetSignedUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadArchive
      expectUploadArchive
      expectQueueArchiveBuilds [Fixtures.firstArchive, Fixtures.secondArchive]
      locators <- archiveUploadSourceUnit DependencyRebuildReuseCache scanDir Fixtures.vendoredDeps
      locators `shouldBe'` Fixtures.locators

expectUploadArchive :: Has MockApi sig m => m ()
expectUploadArchive = do
  UploadArchive Fixtures.signedUrl "test/App/Fossa/VendoredDependency/testdata" `returnsOnceForAnyRequest` "success"

expectQueueArchiveBuild :: Has MockApi sig m => Archive -> m ()
expectQueueArchiveBuild archive =
  QueueArchiveBuild [archive] DependencyRebuildReuseCache `returnsOnce` ()

expectQueueArchiveBuilds :: Has MockApi sig m => [Archive] -> m ()
expectQueueArchiveBuilds archives =
  QueueArchiveBuild archives DependencyRebuildReuseCache `returnsOnce` ()

expectGetSignedUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedUrl packageRevision = GetSignedUploadUrl ArchiveUpload packageRevision `alwaysReturns` Fixtures.signedUrl
