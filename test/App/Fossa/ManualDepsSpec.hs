{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.ManualDepsSpec (
  spec,
)
where

import App.Fossa.Config.Analyze (VendoredDependencyOptions (..))
import App.Fossa.ManualDeps (
  CustomDependency (CustomDependency),
  DependencyMetadata (DependencyMetadata),
  ForkAlias (ForkAlias),
  ForkAliasEntry (ForkAliasEntry),
  LinuxReferenceDependency (..),
  LocatorDependency (..),
  ManagedReferenceDependency (..),
  ManualDependencies (ManualDependencies),
  ReferencedDependency (..),
  RemoteDependency (RemoteDependency),
  VendoredDependency (VendoredDependency),
  collectInteriorLabels,
  getScanCfg,
 )
import App.Fossa.VendoredDependency (VendoredDependencyScanMode (..))
import Control.Effect.Exception (displayException)
import Data.Aeson qualified as Json
import Data.ByteString qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (encodeUtf8, toText)
import Data.Text (Text)
import Data.Yaml qualified as Yaml
import DepTypes (DepType (..))
import Fossa.API.Types (OrgId (OrgId), Organization (..))
import Srclib.Types (Locator (Locator), ProvidedPackageLabel (..), ProvidedPackageLabelScope (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe, shouldContain)
import Test.Hspec.Core.Spec (SpecM)
import Text.RawString.QQ (r)
import Types (ArchiveUploadType (..))

getTestDataFile :: String -> SpecM a BS.ByteString
getTestDataFile name = runIO . BS.readFile $ "test/App/Fossa/testdata/" <> name

theWorks :: ManualDependencies
theWorks = ManualDependencies references customs vendors remotes locators forkAliases
  where
    references =
      [ Managed (ManagedReferenceDependency "one" GemType Nothing [])
      , Managed (ManagedReferenceDependency "two" PipType (Just "1.0.0") [])
      ]
    customs =
      [ CustomDependency "hello" "1.2.3" "MIT" Nothing []
      , CustomDependency "full" "3.2.1" "GPL-3.0" (Just (DependencyMetadata (Just "description for full custom") (Just "we don't validate homepages - custom"))) []
      ]
    remotes =
      [ RemoteDependency "url-dep-one" "1.2.3" "www.url1.tar.gz" (Just (DependencyMetadata (Just "description for url") (Just "we don't validate homepages - url"))) []
      , RemoteDependency "url-dep-two" "1.2.4" "www.url2.tar.gz" Nothing []
      ]
    vendors =
      [ VendoredDependency "vendored" "path" Nothing Nothing []
      , VendoredDependency "versioned" "path/to/dep" (Just "2.1.0") Nothing []
      , VendoredDependency "metadata" "path" (Just "1.1.0") (Just (DependencyMetadata (Just "description for vendored") (Just "we don't validate homepages - vendored"))) []
      ]
    locators =
      [ LocatorDependencyPlain (Locator "fetcher-1" "one" Nothing)
      , LocatorDependencyPlain (Locator "fetcher-2" "two" (Just "1.0.0"))
      ]
    forkAliases =
      [ForkAlias (ForkAliasEntry CargoType "my-serde" Nothing) (ForkAliasEntry CargoType "serde" Nothing) []]

theWorksLabeled :: ManualDependencies
theWorksLabeled = ManualDependencies references customs vendors remotes locators forkAliases
  where
    references =
      [ Managed (ManagedReferenceDependency "one" GemType Nothing [ProvidedPackageLabel "gem-label" ProvidedPackageLabelScopeRevision])
      , Managed (ManagedReferenceDependency "two" PipType (Just "1.0.0") [ProvidedPackageLabel "pypi-label" ProvidedPackageLabelScopeOrg, ProvidedPackageLabel "pypi-label-2" ProvidedPackageLabelScopeProject])
      , LinuxApkDebDep (LinuxReferenceDependency "libssl" LinuxAPK (Just "3.2.1") "x86_64" "alpine" "3.18" [ProvidedPackageLabel "alpine-container" ProvidedPackageLabelScopeOrg])
      , LinuxRpmDep (LinuxReferenceDependency "libcurl" LinuxRPM (Just "7.89.1") "aarch64" "fedora" "38" [ProvidedPackageLabel "fedora-container" ProvidedPackageLabelScopeRevision]) (Just "1")
      ]
    customs =
      [ CustomDependency "hello" "1.2.3" "MIT" Nothing [ProvidedPackageLabel "custom-label-hello" ProvidedPackageLabelScopeOrg]
      , CustomDependency "full" "3.2.1" "GPL-3.0" (Just (DependencyMetadata (Just "description for full custom") (Just "we don't validate homepages - custom"))) [ProvidedPackageLabel "custom-label-full" ProvidedPackageLabelScopeProject]
      ]
    remotes =
      [ RemoteDependency "url-dep-one" "1.2.3" "www.url1.tar.gz" (Just (DependencyMetadata (Just "description for url") (Just "we don't validate homepages - url"))) [ProvidedPackageLabel "url-dep-one-label" ProvidedPackageLabelScopeOrg]
      , RemoteDependency "url-dep-two" "1.2.4" "www.url2.tar.gz" Nothing [ProvidedPackageLabel "url-dep-two-label" ProvidedPackageLabelScopeRevision]
      ]
    vendors =
      [ VendoredDependency "vendored" "path" Nothing Nothing [ProvidedPackageLabel "vendored-dependency-label" ProvidedPackageLabelScopeOrg]
      , VendoredDependency "versioned" "path/to/dep" (Just "2.1.0") Nothing [ProvidedPackageLabel "versioned-dependency-label" ProvidedPackageLabelScopeProject]
      , VendoredDependency "metadata" "path" (Just "1.1.0") (Just (DependencyMetadata (Just "description for vendored") (Just "we don't validate homepages - vendored"))) [ProvidedPackageLabel "metadata-dependency-label" ProvidedPackageLabelScopeRevision]
      ]
    locators =
      [ LocatorDependencyStructured (Locator "fetcher-1" "one" Nothing) [ProvidedPackageLabel "locator-dependency-label" ProvidedPackageLabelScopeOrg]
      , LocatorDependencyStructured (Locator "fetcher-2" "two" (Just "1.0.0")) [ProvidedPackageLabel "locator-dependency-label" ProvidedPackageLabelScopeOrg]
      ]
    forkAliases =
      [ForkAlias (ForkAliasEntry CargoType "my-serde" Nothing) (ForkAliasEntry CargoType "serde" Nothing) []]

theWorksLabels :: Maybe OrgId -> Map Text [ProvidedPackageLabel]
theWorksLabels org =
  Map.fromList $
    referencedLabels
      <> archiveLabels
      <> userLabels
      <> locatorLabels
      <> urlPrivateLabels org
  where
    referencedLabels :: [(Text, [ProvidedPackageLabel])]
    referencedLabels =
      [ ("gem+one$", [ProvidedPackageLabel "gem-label" ProvidedPackageLabelScopeRevision])
      , ("pip+two$1.0.0", [ProvidedPackageLabel "pypi-label" ProvidedPackageLabelScopeOrg, ProvidedPackageLabel "pypi-label-2" ProvidedPackageLabelScopeProject])
      , ("apk+libssl#alpine#3.18$x86_64#3.2.1", [ProvidedPackageLabel "alpine-container" ProvidedPackageLabelScopeOrg])
      , ("rpm-generic+libcurl#fedora#38$aarch64#1:7.89.1", [ProvidedPackageLabel "fedora-container" ProvidedPackageLabelScopeRevision])
      ]

    archiveLabels :: [(Text, [ProvidedPackageLabel])]
    archiveLabels =
      [ ("archive+vendored$", [ProvidedPackageLabel "vendored-dependency-label" ProvidedPackageLabelScopeOrg])
      , ("archive+versioned$2.1.0", [ProvidedPackageLabel "versioned-dependency-label" ProvidedPackageLabelScopeProject])
      , ("archive+metadata$1.1.0", [ProvidedPackageLabel "metadata-dependency-label" ProvidedPackageLabelScopeRevision])
      ]

    userLabels :: [(Text, [ProvidedPackageLabel])]
    userLabels =
      [ ("user+hello$1.2.3", [ProvidedPackageLabel "custom-label-hello" ProvidedPackageLabelScopeOrg])
      , ("user+full$3.2.1", [ProvidedPackageLabel "custom-label-full" ProvidedPackageLabelScopeProject])
      ]

    locatorLabels :: [(Text, [ProvidedPackageLabel])]
    locatorLabels =
      [ ("fetcher-1+one$", [ProvidedPackageLabel "locator-dependency-label" ProvidedPackageLabelScopeOrg])
      , ("fetcher-2+two$1.0.0", [ProvidedPackageLabel "locator-dependency-label" ProvidedPackageLabelScopeOrg])
      ]

    urlPrivateLabels :: Maybe OrgId -> [(Text, [ProvidedPackageLabel])]
    urlPrivateLabels Nothing =
      [ ("url-private+www.url1.tar.gz$1.2.3", [ProvidedPackageLabel "url-dep-one-label" ProvidedPackageLabelScopeOrg])
      , ("url-private+www.url2.tar.gz$1.2.4", [ProvidedPackageLabel "url-dep-two-label" ProvidedPackageLabelScopeRevision])
      ]
    urlPrivateLabels (Just orgId) =
      [ ("url-private+" <> toText orgId <> "/www.url1.tar.gz$1.2.3", [ProvidedPackageLabel "url-dep-one-label" ProvidedPackageLabelScopeOrg])
      , ("url-private+" <> toText orgId <> "/www.url2.tar.gz$1.2.4", [ProvidedPackageLabel "url-dep-two-label" ProvidedPackageLabelScopeRevision])
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
    it "should parse json correctly" $
      case Json.eitherDecodeStrict' theWorksBS of
        Left err -> expectationFailure err
        Right jsonDeps -> jsonDeps `shouldBe` theWorks

  describe "fossa-deps labeled" $ do
    theWorksLabeledBS <- getTestDataFile "the-works-labeled.yml"
    it "should successfully parse all possible inputs" $
      case Yaml.decodeEither' theWorksLabeledBS of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> yamlDeps `shouldBe` theWorksLabeled

    it "should extract nested labels with orgId" $
      case Yaml.decodeEither' theWorksLabeledBS of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> do
          let orgId = OrgId 1234
          let extracted = collectInteriorLabels (Just orgId) yamlDeps
          extracted `shouldBe` theWorksLabels (Just orgId)

    it "should extract nested labels without orgId" $
      case Yaml.decodeEither' theWorksLabeledBS of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> do
          let extracted = collectInteriorLabels Nothing yamlDeps
          extracted `shouldBe` theWorksLabels Nothing

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

    referenceDepSpec
    remoteDepSpec
    customDepSpec
    vendorDepSpec
    locatorDepSpec
    forkAliasSpec

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
    it "should parse linux reference dependency" $
      case Yaml.decodeEither' (encodeUtf8 linuxReferenceDep) of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> yamlDeps `shouldBe` linuxRefManualDep "centos" Nothing

    it "should parse rpm reference dependency with epoch" $
      case Yaml.decodeEither' (encodeUtf8 linuxReferenceDepWithEpoch) of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> yamlDeps `shouldBe` linuxRefManualDep "centos" (Just "1")

    it "should fail when linux reference dependency of deb or apk contains epoch" $
      exceptionContains
        (encodeUtf8 apkReferenceDepWithEpoch)
        "Invalid field name for referenced dependencies (of dependency type: apk): epoch"

    it "should fail when linux reference dependency does not include arch information" $
      exceptionContains
        (encodeUtf8 linuxReferenceDepWithoutArch)
        "arch is required field for reference dependency (of dependency type: apk, deb, rpm-generic)"

    it "should fail when linux reference dependency does not include os information" $
      exceptionContains
        (encodeUtf8 linuxReferenceDepWithoutOS)
        "os is required field for reference dependency (of dependency type: apk, deb, rpm-generic)"

    it "should fail when linux reference dependency uses not supported os" $
      exceptionContains
        (encodeUtf8 linuxReferenceDepWithUnsupportedOS)
        "Provided os: poky is not supported! Please provide oneOf:"

    it "should fail when managed reference dependency provides os information" $
      exceptionContains
        (encodeUtf8 managedReferenceDepWithOS)
        "Invalid field name for referenced dependencies (of dependency type: gem): os"

    it "should fail when linux reference dependency of deb or apk contains epoch" $
      exceptionContains
        (encodeUtf8 managedReferenceDepWithEmptyName)
        "expected field 'name' to be non-empty"

remoteDepSpec :: Spec
remoteDepSpec = do
  describe "remote dependency" $ do
    it "should fail when remote dependency has empty name or only whitespace" $
      exceptionContains
        (encodeUtf8 remoteDepWithEmptyName)
        "expected field 'name' to be non-empty"

    it "should fail when remote dependency has empty version or only whitespace" $
      exceptionContains
        (encodeUtf8 remoteDepWithEmptyVersion)
        "expected field 'version' to be non-empty"

    it "should fail when remote dependency has empty url or only whitespace" $
      exceptionContains
        (encodeUtf8 remoteDepWithEmptyUrl)
        "expected field 'url' to be non-empty"

customDepSpec :: Spec
customDepSpec = do
  describe "custom dependency" $ do
    it "should fail when custom dependency has empty name or only whitespace" $
      exceptionContains
        (encodeUtf8 customDepWithEmptyName)
        "expected field 'name' to be non-empty"

    it "should fail when custom dependency has empty version or only whitespace" $
      exceptionContains
        (encodeUtf8 customDepWithEmptyVersion)
        "expected field 'version' to be non-empty"

    it "should fail when custom dependency has empty license or only whitespace" $
      exceptionContains
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
    it "should fail when locator dependency is empty" $
      exceptionContains
        (encodeUtf8 locatorDepWithEmptyDep)
        "parsing Locator failed, expected String, but encountered Null"

forkAliasSpec :: Spec
forkAliasSpec = do
  describe "fork alias" $ do
    it "should parse fork alias" $
      case Yaml.decodeEither' (encodeUtf8 forkAliasDep) of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> yamlDeps `shouldBe` forkAliasManualDep

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
    [LinuxRpmDep (LinuxReferenceDependency "pkgName" LinuxRPM (Just "1.1") "x86" os "2.2" []) epoch]
    mempty
    mempty
    mempty
    mempty
    mempty

forkAliasManualDep :: ManualDependencies
forkAliasManualDep =
  ManualDependencies
    mempty
    mempty
    mempty
    mempty
    mempty
    [ForkAlias (ForkAliasEntry CargoType "my-serde" Nothing) (ForkAliasEntry CargoType "serde" Nothing) []]

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
-
|]

forkAliasDep :: Text
forkAliasDep =
  [r|
fork-aliases:
- fork:
    type: cargo
    name: my-serde
  base:
    type: cargo
    name: serde
|]
