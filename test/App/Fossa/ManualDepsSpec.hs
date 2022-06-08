{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.ManualDepsSpec (
  spec,
) where

import App.Fossa.Config.Analyze (VendoredDependencyOptions (..))
import App.Fossa.ManualDeps (
  CustomDependency (CustomDependency),
  DependencyMetadata (DependencyMetadata),
  ManualDependencies (ManualDependencies),
  ReferencedDependency (ReferencedDependency),
  RemoteDependency (RemoteDependency),
  VendoredDependency (VendoredDependency),
  scanAndUpload,
 )
import Control.Effect.Exception (Has, displayException)
import Control.Effect.FossaApiClient (FossaApiClientF (FinalizeLicenseScan, GetAnalyzedRevisions, GetApiOpts, GetOrganization, GetSignedLicenseScanUrl, GetSignedUploadUrl, QueueArchiveBuild, UploadArchive, UploadLicenseScanResult), PackageRevision (..))
import Data.Aeson qualified as Json
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Yaml qualified as Yaml
import DepTypes (DepType (..))
import Fossa.API.Types (Archive, ArchiveComponents (..), Organization (..))
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Srclib.Types (LicenseSourceUnit, renderLocator)
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe, shouldContain)
import Test.Hspec.Core.Spec (SpecM)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce, returnsOnceForAnyRequest)
import Types (ArchiveUploadType (..))

getTestDataFile :: String -> SpecM a BS.ByteString
getTestDataFile name = runIO . BS.readFile $ "test/App/Fossa/testdata/" <> name

theWorks :: ManualDependencies
theWorks = ManualDependencies references customs vendors remotes
  where
    references =
      [ ReferencedDependency "one" GemType Nothing
      , ReferencedDependency "two" PipType $ Just "1.0.0"
      ]
    customs =
      [ CustomDependency "hello" "1.2.3" "MIT" Nothing
      , CustomDependency "full" "3.2.1" "GPL-3.0" (Just (DependencyMetadata (Just "description for full custom") (Just "we don't validate homepages - custom")))
      ]
    remotes =
      [ RemoteDependency "url-dep-one" "1.2.3" "www.url1.tar.gz" (Just (DependencyMetadata (Just "description for url") (Just "we don't validate homepages - url")))
      , RemoteDependency "url-dep-two" "1.2.4" "www.url2.tar.gz" Nothing
      ]
    vendors =
      [ VendoredDependency "vendored" "path" Nothing
      , VendoredDependency "versioned" "path/to/dep" (Just "2.1.0")
      ]

exceptionContains :: BS.ByteString -> String -> Expectation
exceptionContains yamlBytes partial = case Yaml.decodeEither' @ManualDependencies yamlBytes of
  -- Ethics issue: right is wrong
  Right _ -> expectationFailure $ "Expected to fail with message containing: " <> partial
  Left exc -> displayException exc `shouldContain` partial

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/VendoredDependency/testdata/repo")

spec :: Spec
spec = do
  describe "fossa-deps json parser" $ do
    theWorksBS <- getTestDataFile "the-works.json"
    it "should parse json correctly" $
      case Json.eitherDecodeStrict' theWorksBS of
        Left err -> expectationFailure err
        Right jsonDeps -> jsonDeps `shouldBe` theWorks

  describe "fossa-deps yaml parser" $ do
    theWorksBS <- getTestDataFile "the-works.yml"
    it "should successfully parse all possible inputs" $
      case Yaml.decodeEither' theWorksBS of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> yamlDeps `shouldBe` theWorks

    unsupportedTypeBS <- getTestDataFile "unsupported-type.yml"
    it "should report an unsupported type" $ exceptionContains unsupportedTypeBS "dep type: notafetcher not supported"

    licenseInRefDepBS <- getTestDataFile "license-in-ref-dep.yml"
    it "should report license used on referenced deps" $
      exceptionContains licenseInRefDepBS "Invalid field name for referenced dependencies: license"

  describe "scanAndUpload" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir

    it' "should fail if you try to force a license scan but FOSSA does not support it" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Just CLILicenseScan}
      expectGetNoCliScanOrganization
      expectGetApiOpts
      expectFatal' $ scanAndUpload scanDir Fixtures.vendoredDeps opts

    it' "should do a license scan if requested and FOSSA supports it" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Just CLILicenseScan}
      expectGetStandardOrganization
      expectGetApiOpts
      expectEverythingScannedAlready
      expectFinalizeScan Fixtures.archives
      locators <- scanAndUpload scanDir Fixtures.vendoredDeps opts
      locators `shouldBe'` Fixtures.locators

    it' "should do a license scan if they are the default and no flags are passed" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Nothing}
      expectGetStandardOrganization
      expectGetApiOpts
      expectEverythingScannedAlready
      expectFinalizeScan Fixtures.archives
      locators <- scanAndUpload scanDir Fixtures.vendoredDeps opts
      locators `shouldBe'` Fixtures.locators

    it' "should force a license scan rebuild if forceRescans is True" $ do
      let opts = VendoredDependencyOptions{forceRescans = True, licenseScanMethod = Nothing}
      expectGetStandardOrganization
      expectGetApiOpts
      expectGetSignedLicenseScanUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectGetSignedLicenseScanUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.secondLicenseSourceUnit
      expectFinalizeScanWithForceRebuild Fixtures.archives
      locators <- scanAndUpload scanDir Fixtures.vendoredDeps opts
      locators `shouldBe'` Fixtures.locators

    it' "should do an archive upload if they are the default and no flags are passed" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Nothing}
      expectGetArchiveUploadOrganization
      expectGetApiOpts
      expectGetSignedArchiveUploadUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectGetSignedArchiveUploadUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadArchive
      expectUploadArchive
      expectQueueArchiveBuild Fixtures.firstArchive
      expectQueueArchiveBuild Fixtures.secondArchive
      locators <- scanAndUpload scanDir Fixtures.vendoredDeps opts
      locators `shouldBe'` Fixtures.locators

    it' "should do an archive upload if requested and CLI license scan is the default" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Just ArchiveUpload}
      expectGetArchiveUploadOrganization
      expectGetApiOpts
      expectGetSignedArchiveUploadUrl PackageRevision{packageName = "first-archive-test", packageVersion = "0.0.1"}
      expectGetSignedArchiveUploadUrl PackageRevision{packageName = "second-archive-test", packageVersion = "0.0.1"}
      expectUploadArchive
      expectUploadArchive
      expectQueueArchiveBuild Fixtures.firstArchive
      expectQueueArchiveBuild Fixtures.secondArchive
      locators <- scanAndUpload scanDir Fixtures.vendoredDeps opts
      locators `shouldBe'` Fixtures.locators

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts =
  GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetStandardOrganization :: Has MockApi sig m => m ()
expectGetStandardOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectGetNoCliScanOrganization :: Has MockApi sig m => m ()
expectGetNoCliScanOrganization = GetOrganization `alwaysReturns` Fixtures.organization{orgCoreSupportsLocalLicenseScan = False}

expectGetArchiveUploadOrganization :: Has MockApi sig m => m ()
expectGetArchiveUploadOrganization = GetOrganization `alwaysReturns` Fixtures.organization{orgDefaultVendoredDependencyScanType = ArchiveUpload}

expectGetSignedLicenseScanUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedLicenseScanUrl packageRevision = GetSignedLicenseScanUrl packageRevision `alwaysReturns` Fixtures.signedUrl

expectUploadLicenseScanResult :: Has MockApi sig m => LicenseSourceUnit -> m ()
expectUploadLicenseScanResult licenseUnit =
  (UploadLicenseScanResult Fixtures.signedUrl licenseUnit) `returnsOnceForAnyRequest` ()

expectEverythingScannedAlready :: Has MockApi sig m => m ()
expectEverythingScannedAlready =
  GetAnalyzedRevisions Fixtures.vendoredDeps
    `returnsOnce` map renderLocator (NE.toList Fixtures.locators)

expectFinalizeScan :: Has MockApi sig m => [Archive] -> m ()
expectFinalizeScan as =
  (FinalizeLicenseScan ArchiveComponents{archives = as, forceRebuild = False}) `returnsOnce` ()

expectFinalizeScanWithForceRebuild :: Has MockApi sig m => [Archive] -> m ()
expectFinalizeScanWithForceRebuild as =
  (FinalizeLicenseScan ArchiveComponents{archives = as, forceRebuild = True}) `returnsOnce` ()

expectGetSignedArchiveUploadUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedArchiveUploadUrl packageRevision = GetSignedUploadUrl packageRevision `alwaysReturns` Fixtures.signedUrl

expectUploadArchive :: Has MockApi sig m => m ()
expectUploadArchive = do
  UploadArchive Fixtures.signedUrl "test/App/Fossa/VendoredDependency/testdata" `returnsOnceForAnyRequest` "success"

expectQueueArchiveBuild :: Has MockApi sig m => Archive -> m ()
expectQueueArchiveBuild archive =
  QueueArchiveBuild archive `returnsOnce` pure "success"
