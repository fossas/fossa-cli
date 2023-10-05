{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.ManualDepsSpec (
  spec,
)
where

import App.Fossa.Config.Analyze (VendoredDependencyOptions (..))
import App.Fossa.ManualDeps (
  CustomDependency (CustomDependency),
  DependencyMetadata (DependencyMetadata),
  LinuxReferenceDependency (..),
  ManagedReferenceDependency (..),
  ManualDependencies (ManualDependencies),
  ReferencedDependency (..),
  RemoteDependency (RemoteDependency),
  VendoredDependency (VendoredDependency),
  getScanCfg,
 )
import App.Fossa.VendoredDependency (VendoredDependencyScanMode (..))
import Control.Effect.Exception (displayException)
import Data.Aeson qualified as Json
import Data.ByteString qualified as BS
import Data.String.Conversion (encodeUtf8)
import Data.Text (Text)
import Data.Yaml qualified as Yaml
import DepTypes (DepType (..))
import Fossa.API.Types (Organization (..))
import Srclib.Types (Locator (Locator))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe, shouldContain)
import Test.Hspec.Core.Spec (SpecM)
import Text.RawString.QQ (r)
import Types (ArchiveUploadType (..))

getTestDataFile :: String -> SpecM a BS.ByteString
getTestDataFile name = runIO . BS.readFile $ "test/App/Fossa/testdata/" <> name

theWorks :: ManualDependencies
theWorks = ManualDependencies references customs vendors remotes locators
  where
    references =
      [ Managed (ManagedReferenceDependency "one" GemType Nothing)
      , Managed (ManagedReferenceDependency "two" PipType $ Just "1.0.0")
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
    locators =
      [ Locator "fetcher-1" "one" Nothing
      , Locator "fetcher-2" "two" $ Just "1.0.0"
      ]

exceptionContains :: BS.ByteString -> String -> Expectation
exceptionContains yamlBytes partial = case Yaml.decodeEither' @ManualDependencies yamlBytes of
  -- Ethics issue: right is wrong
  Right _ -> expectationFailure $ "Expected to fail with message containing: " <> partial
  Left exc -> displayException exc `shouldContain` partial

spec :: Spec
spec = do
  describe "fossa-deps json parser" $ do
    theWorksBS <- getTestDataFile "the-works.json"
    it "should parse json correctly"
      $ case Json.eitherDecodeStrict' theWorksBS of
        Left err -> expectationFailure err
        Right jsonDeps -> jsonDeps `shouldBe` theWorks

  describe "fossa-deps yaml parser" $ do
    theWorksBS <- getTestDataFile "the-works.yml"
    it "should successfully parse all possible inputs"
      $ case Yaml.decodeEither' theWorksBS of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> yamlDeps `shouldBe` theWorks

    unsupportedTypeBS <- getTestDataFile "unsupported-type.yml"
    it "should report an unsupported type" $ exceptionContains unsupportedTypeBS "dep type: notafetcher not supported"

    licenseInRefDepBS <- getTestDataFile "license-in-ref-dep.yml"
    it "should report license used on referenced deps"
      $ exceptionContains licenseInRefDepBS "Invalid field name for referenced dependencies: license"

    referenceDepSpec
    remoteDepSpec
    customDepSpec
    vendorDepSpec
    locatorDepSpec

  describe "getScanCfg" $ do
    it' "should fail if you try to force a license scan but the FOSSA server does not support it" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Just CLILicenseScan, licenseScanPathFilters = Nothing}
          org = Fixtures.organization{orgCoreSupportsLocalLicenseScan = False}
      expectFatal' $ getScanCfg org opts

    it' "should do a license scan if requested and FOSSA supports it" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Just CLILicenseScan, licenseScanPathFilters = Nothing}
      (uploadType, scanMode) <- getScanCfg Fixtures.organization opts
      (uploadType, scanMode) `shouldBe'` (CLILicenseScan, SkipPreviouslyScanned)

    it' "should do a license scan if they are the default and no flags are passed" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Nothing, licenseScanPathFilters = Nothing}
      (uploadType, scanMode) <- getScanCfg Fixtures.organization opts
      (uploadType, scanMode) `shouldBe'` (CLILicenseScan, SkipPreviouslyScanned)

    it' "should force a license scan rebuild if forceRescans is True" $ do
      let opts = VendoredDependencyOptions{forceRescans = True, licenseScanMethod = Nothing, licenseScanPathFilters = Nothing}
      (uploadType, scanMode) <- getScanCfg Fixtures.organization opts
      (uploadType, scanMode) `shouldBe'` (CLILicenseScan, SkippingDisabledViaFlag)

    it' "should not skip if the server does not support the analyzed revisions query" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Nothing, licenseScanPathFilters = Nothing}
          org = Fixtures.organization{orgSupportsAnalyzedRevisionsQuery = False}
      (uploadType, scanMode) <- getScanCfg org opts
      (uploadType, scanMode) `shouldBe'` (CLILicenseScan, SkippingNotSupported)

    it' "should do an archive upload if they are the default and no flags are passed" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Nothing, licenseScanPathFilters = Nothing}
          org = Fixtures.organization{orgDefaultVendoredDependencyScanType = ArchiveUpload}
      (uploadType, scanMode) <- getScanCfg org opts
      (uploadType, scanMode) `shouldBe'` (ArchiveUpload, SkipPreviouslyScanned)

    it' "should do an archive upload if requested and CLI license scan is the default" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Just ArchiveUpload, licenseScanPathFilters = Nothing}
          org = Fixtures.organization{orgDefaultVendoredDependencyScanType = ArchiveUpload}
      (uploadType, scanMode) <- getScanCfg org opts
      (uploadType, scanMode) `shouldBe'` (ArchiveUpload, SkipPreviouslyScanned)

referenceDepSpec :: Spec
referenceDepSpec = do
  describe "reference dependency" $ do
    it "should parse linux reference dependency"
      $ case Yaml.decodeEither' (encodeUtf8 linuxReferenceDep) of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> yamlDeps `shouldBe` linuxRefManualDep "centos" Nothing

    it "should parse rpm reference dependency with epoch"
      $ case Yaml.decodeEither' (encodeUtf8 linuxReferenceDepWithEpoch) of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> yamlDeps `shouldBe` linuxRefManualDep "centos" (Just "1")

    it "should fail when linux reference dependency of deb or apk contains epoch"
      $ exceptionContains
        (encodeUtf8 apkReferenceDepWithEpoch)
        "Invalid field name for referenced dependencies (of dependency type: apk): epoch"

    it "should fail when linux reference dependency does not include arch information"
      $ exceptionContains
        (encodeUtf8 linuxReferenceDepWithoutArch)
        "arch is required field for reference dependency (of dependency type: apk, deb, rpm-generic)"

    it "should fail when linux reference dependency does not include os information"
      $ exceptionContains
        (encodeUtf8 linuxReferenceDepWithoutOS)
        "os is required field for reference dependency (of dependency type: apk, deb, rpm-generic)"

    it "should fail when linux reference dependency uses not supported os"
      $ exceptionContains
        (encodeUtf8 linuxReferenceDepWithUnsupportedOS)
        "Provided os: poky is not supported! Please provide oneOf:"

    it "should fail when managed reference dependency provides os information"
      $ exceptionContains
        (encodeUtf8 managedReferenceDepWithOS)
        "Invalid field name for referenced dependencies (of dependency type: gem): os"

    it "should fail when linux reference dependency of deb or apk contains epoch"
      $ exceptionContains
        (encodeUtf8 managedReferenceDepWithEmptyName)
        "expected field 'name' to be non-empty"

remoteDepSpec :: Spec
remoteDepSpec = do
  describe "remote dependency" $ do
    it "should fail when remote dependency has empty name or only whitespace"
      $ exceptionContains
        (encodeUtf8 remoteDepWithEmptyName)
        "expected field 'name' to be non-empty"

    it "should fail when remote dependency has empty version or only whitespace"
      $ exceptionContains
        (encodeUtf8 remoteDepWithEmptyVersion)
        "expected field 'version' to be non-empty"

    it "should fail when remote dependency has empty url or only whitespace"
      $ exceptionContains
        (encodeUtf8 remoteDepWithEmptyUrl)
        "expected field 'url' to be non-empty"

customDepSpec :: Spec
customDepSpec = do
  describe "custom dependency" $ do
    it "should fail when custom dependency has empty name or only whitespace"
      $ exceptionContains
        (encodeUtf8 customDepWithEmptyName)
        "expected field 'name' to be non-empty"

    it "should fail when custom dependency has empty version or only whitespace"
      $ exceptionContains
        (encodeUtf8 customDepWithEmptyVersion)
        "expected field 'version' to be non-empty"

    it "should fail when custom dependency has empty license or only whitespace"
      $ exceptionContains
        (encodeUtf8 customDepWithEmptyLicense)
        "expected field 'license' to be non-empty"

vendorDepSpec :: Spec
vendorDepSpec = do
  describe "vendor dependency" $ do
    it "should fail when vendor dependency has empty name or only whitespace" $ do
      exceptionContains
        (encodeUtf8 vendorDepWithEmptyName)
        "expected field 'name' to be non-empty"

    it "should fail when vendor dependency has empty path or only whitespace" $ do
      exceptionContains
        (encodeUtf8 vendorDepWithEmptyPath)
        "expected field 'path' to be non-empty"

    it "should fail when vendor dependency has version with not supported character" $ do
      exceptionContains
        (encodeUtf8 vendorDepWithHashtagInVersion)
        "field 'version' conatins forbidden character(s): [\"#\"]"

      exceptionContains
        (encodeUtf8 vendorDepWithQuestionInVersion)
        "field 'version' conatins forbidden character(s): [\"?\"]"

locatorDepSpec :: Spec
locatorDepSpec = do
  describe "locator dependency" $ do
    it "should fail when locator dependency is empty"
      $ exceptionContains
        (encodeUtf8 locatorDepWithEmptyDep)
        "expected locator dependency to be non-empty"

linuxReferenceDep :: Text
linuxReferenceDep =
  [r|
referenced-dependencies:
- name: pkgName
  type: rpm-generic
  version: 1.1
  arch: x86
  os: centos
  osVersion: 2.2
|]

apkReferenceDepWithEpoch :: Text
apkReferenceDepWithEpoch =
  [r|
referenced-dependencies:
- name: pkgName
  type: apk
  version: 1.1
  epoch: 1
  arch: x86
  os: centos
  osVersion: 2.2
|]

linuxReferenceDepWithEpoch :: Text
linuxReferenceDepWithEpoch =
  [r|
referenced-dependencies:
- name: pkgName
  type: rpm-generic
  version: 1.1
  epoch: 1
  arch: x86
  os: centos
  osVersion: 2.2
|]

linuxReferenceDepWithoutArch :: Text
linuxReferenceDepWithoutArch =
  [r|
referenced-dependencies:
- name: pkgName
  type: rpm-generic
  version: 1.1
  os: centos
  osVersion: 2.2
|]

linuxReferenceDepWithoutOS :: Text
linuxReferenceDepWithoutOS =
  [r|
referenced-dependencies:
- name: pkgName
  type: rpm-generic
  version: 1.1
  arch: x86
|]

linuxReferenceDepWithUnsupportedOS :: Text
linuxReferenceDepWithUnsupportedOS =
  [r|
referenced-dependencies:
- name: pkgName
  type: rpm-generic
  version: 1.1
  arch: x86
  os: poky
  osVersion: 2.2
|]

managedReferenceDepWithOS :: Text
managedReferenceDepWithOS =
  [r|
referenced-dependencies:
- name: one
  type: gem
  os: poky
  osVersion: 2.2
|]

linuxRefManualDep :: Text -> Maybe Text -> ManualDependencies
linuxRefManualDep os epoch =
  ManualDependencies
    [LinuxRpmDep (LinuxReferenceDependency "pkgName" LinuxRPM (Just "1.1") "x86" os "2.2") epoch]
    mempty
    mempty
    mempty
    mempty

customDepWithEmptyVersion :: Text
customDepWithEmptyVersion =
  [r|
custom-dependencies:
- name: example
  version: " "
  license: "mit"
|]

customDepWithEmptyName :: Text
customDepWithEmptyName =
  [r|
custom-dependencies:
- name: ""
  version: 1.0
  license: "mit"
|]

customDepWithEmptyLicense :: Text
customDepWithEmptyLicense =
  [r|
custom-dependencies:
- name: example
  version: 1.0
  license: " "
|]

remoteDepWithEmptyName :: Text
remoteDepWithEmptyName =
  [r|
remote-dependencies:
- name: " "
  version: "1"
  url: "https://github.com/fossas/fossa-cli/archive/refs/heads/master.zip"
|]

remoteDepWithEmptyVersion :: Text
remoteDepWithEmptyVersion =
  [r|
remote-dependencies:
- name: example
  version: " "
  url: "https://github.com/fossas/fossa-cli/archive/refs/heads/master.zip"
|]

remoteDepWithEmptyUrl :: Text
remoteDepWithEmptyUrl =
  [r|
remote-dependencies:
- name: example
  version: "1.0.0"
  url: " "
|]

vendorDepWithHashtagInVersion :: Text
vendorDepWithHashtagInVersion =
  [r|
vendored-dependencies:
- name: example
  version: "1#1"
  path: .
|]

vendorDepWithQuestionInVersion :: Text
vendorDepWithQuestionInVersion =
  [r|
vendored-dependencies:
- name: example
  version: "1?os=linux"
  path: .
|]

vendorDepWithEmptyName :: Text
vendorDepWithEmptyName =
  [r|
vendored-dependencies:
- name: " "
  version: "1"
  path: .
|]

vendorDepWithEmptyPath :: Text
vendorDepWithEmptyPath =
  [r|
vendored-dependencies:
- name: example
  version: "1"
  path: " "
|]

managedReferenceDepWithEmptyName :: Text
managedReferenceDepWithEmptyName =
  [r|
referenced-dependencies:
- name: " "
  type: gem
|]

locatorDepWithEmptyDep :: Text
locatorDepWithEmptyDep =
  [r|
locator-dependencies:
- " "
|]
