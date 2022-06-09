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
  getScanCfg,
 )
import App.Fossa.VendoredDependency (VendoredDependencyScanMode (..))
import Control.Effect.Exception (displayException)
import Data.Aeson qualified as Json
import Data.ByteString qualified as BS
import Data.Yaml qualified as Yaml
import DepTypes (DepType (..))
import Fossa.API.Types (Organization (..))
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe, shouldContain)
import Test.Hspec.Core.Spec (SpecM)
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

  describe "getScanCfg" $ do
    it' "should fail if you try to force a license scan but the FOSSA server does not support it" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Just CLILicenseScan}
          org = Fixtures.organization{orgCoreSupportsLocalLicenseScan = False}
      expectFatal' $ getScanCfg org opts

    it' "should do a license scan if requested and FOSSA supports it" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Just CLILicenseScan}
      (uploadType, scanMode) <- getScanCfg Fixtures.organization opts
      (uploadType, scanMode) `shouldBe'` (CLILicenseScan, SkipPreviouslyScanned)

    it' "should do a license scan if they are the default and no flags are passed" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Nothing}
      (uploadType, scanMode) <- getScanCfg Fixtures.organization opts
      (uploadType, scanMode) `shouldBe'` (CLILicenseScan, SkipPreviouslyScanned)

    it' "should force a license scan rebuild if forceRescans is True" $ do
      let opts = VendoredDependencyOptions{forceRescans = True, licenseScanMethod = Nothing}
      (uploadType, scanMode) <- getScanCfg Fixtures.organization opts
      (uploadType, scanMode) `shouldBe'` (CLILicenseScan, SkippingDisabledViaFlag)

    it' "should not skip if the server does not support the analyzed revisions query" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Nothing}
          org = Fixtures.organization{orgSupportsAnalyzedRevisionsQuery = False}
      (uploadType, scanMode) <- getScanCfg org opts
      (uploadType, scanMode) `shouldBe'` (CLILicenseScan, SkippingNotSupported)

    it' "should do an archive upload if they are the default and no flags are passed" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Nothing}
          org = Fixtures.organization{orgDefaultVendoredDependencyScanType = ArchiveUpload}
      (uploadType, scanMode) <- getScanCfg org opts
      (uploadType, scanMode) `shouldBe'` (ArchiveUpload, SkipPreviouslyScanned)

    it' "should do an archive upload if requested and CLI license scan is the default" $ do
      let opts = VendoredDependencyOptions{forceRescans = False, licenseScanMethod = Just ArchiveUpload}
          org = Fixtures.organization{orgDefaultVendoredDependencyScanType = ArchiveUpload}
      (uploadType, scanMode) <- getScanCfg org opts
      (uploadType, scanMode) `shouldBe'` (ArchiveUpload, SkipPreviouslyScanned)
