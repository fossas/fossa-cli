{-# LANGUAGE GADTs #-}

module App.Fossa.Analyze.UploadSpec (spec) where

import App.Fossa.Analyze.Upload (ScanUnits (..), mergeSourceAndLicenseUnits, uploadSuccessfulAnalysis)
import App.Fossa.Config.Analyze (JsonOutput (JsonOutput))
import App.Types (FileUpload (..), LocatorType (..))
import Control.Algebra (Has)
import Control.Carrier.Git (GitC)
import Control.Carrier.Simple (interpret)
import Control.Effect.Diagnostics (fatalText)
import Control.Effect.FossaApiClient (FossaApiClientF (..), PackageRevision (..))
import Control.Effect.Git (GitF (FetchGitContributors))
import Data.Flag (toFlag)
import Data.List.NonEmpty qualified as NE
import Fossa.API.Types (Organization (..), Project (..), UploadResponse (..))
import Srclib.Types (FullSourceUnit (..), LicenseUnitInfo (..), Locator, emptyLicenseUnitData)
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.Hspec.Core.Spec (runIO)
import Test.MockApi (
  MockApi,
  alwaysReturns,
  fails,
  returnsOnce,
  returnsOnceForAnyRequest,
 )
import Types (GraphBreadth (Complete))

withGit :: (forall x. GitF x -> m x) -> GitC m a -> m a
withGit = interpret

mockGit :: Applicative m => GitF a -> m a
mockGit (FetchGitContributors{}) = pure Fixtures.contributors

expectedLocator :: Locator
expectedLocator = uploadLocator Fixtures.uploadResponse

expectGetSuccess :: Has MockApi sig m => m ()
expectGetSuccess = do
  GetProject Fixtures.projectRevision LocatorTypeCustom `alwaysReturns` Fixtures.project
  GetOrganization `alwaysReturns` Fixtures.organization
  GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetProject :: Has MockApi sig m => m ()
expectGetProject = GetProject Fixtures.projectRevision LocatorTypeCustom `alwaysReturns` Fixtures.project

expectGetOrganizationWithFullFileUploads :: Has MockApi sig m => m ()
expectGetOrganizationWithFullFileUploads =
  GetOrganization `alwaysReturns` Fixtures.organization{orgRequiresFullFileUploads = True}

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts = GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectAnalysisUploadSuccess :: Has MockApi sig m => m ()
expectAnalysisUploadSuccess = UploadAnalysis Fixtures.projectRevision Fixtures.projectMetadata Fixtures.sourceUnits `alwaysReturns` Fixtures.uploadResponse

expectContributorUploadSuccess :: Has MockApi sig m => m ()
expectContributorUploadSuccess =
  UploadContributors expectedLocator Fixtures.contributors `alwaysReturns` ()

expectFirstPartyAnalysisUploadSuccess :: FileUpload -> Has MockApi sig m => m ()
expectFirstPartyAnalysisUploadSuccess uploadKind = do
  UploadAnalysisWithFirstPartyLicenses Fixtures.projectRevision Fixtures.projectMetadata uploadKind `alwaysReturns` Fixtures.uploadResponse

expectGetFirstPartySignedUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetFirstPartySignedUrl packageRevision = GetSignedFirstPartyScanUrl packageRevision `alwaysReturns` Fixtures.signedUrl

expectUploadFirstPartyDataToS3 :: Has MockApi sig m => m ()
expectUploadFirstPartyDataToS3 = do
  let mergedUnits = mergeSourceAndLicenseUnits Fixtures.sourceUnits Fixtures.firstLicenseSourceUnit
  UploadFirstPartyScanResult Fixtures.signedUrl mergedUnits `returnsOnceForAnyRequest` ()

expectedMergedFullSourceUnits :: NE.NonEmpty FullSourceUnit
expectedMergedFullSourceUnits = NE.fromList [fullSourceUnit, fullLicenseUnit]
  where
    fullSourceUnit =
      FullSourceUnit
        { fullSourceUnitName = "testSourceUnitName"
        , fullSourceUnitType = "testSourceUnitType"
        , fullSourceUnitTitle = Nothing
        , fullSourceUnitManifest = Just "testSourceUnitManifest"
        , fullSourceUnitBuild = Nothing
        , fullSourceUnitGraphBreadth = Complete
        , fullSourceUnitOriginPaths = []
        , fullSourceUnitAdditionalData = Nothing
        , fullSourceUnitFiles = Nothing
        , fullSourceUnitData = Nothing
        , fullSourceUnitInfo = Nothing
        , fullSourceUnitNoticeFiles = Nothing
        }
    fullLicenseUnit =
      FullSourceUnit
        { fullSourceUnitName = "empty"
        , fullSourceUnitType = "LicenseUnit"
        , fullSourceUnitTitle = Nothing
        , fullSourceUnitManifest = Nothing
        , fullSourceUnitBuild = Nothing
        , fullSourceUnitGraphBreadth = Complete
        , fullSourceUnitOriginPaths = []
        , fullSourceUnitAdditionalData = Nothing
        , fullSourceUnitFiles = Just $ "" NE.:| []
        , fullSourceUnitData = Just $ emptyLicenseUnitData NE.:| []
        , fullSourceUnitInfo = Just LicenseUnitInfo{licenseUnitInfoDescription = Nothing}
        , fullSourceUnitNoticeFiles = Nothing
        }

expectGetSuccessWithReachability :: Has MockApi sig m => m ()
expectGetSuccessWithReachability = do
  GetProject Fixtures.projectRevision LocatorTypeCustom `alwaysReturns` Fixtures.project
  GetOrganization `alwaysReturns` Fixtures.organization{orgSupportsReachability = True}
  GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectReachabilityBuildUpload :: (Has MockApi sig m) => m ()
expectReachabilityBuildUpload =
  UploadBuildForReachability
    Fixtures.projectRevision
    Fixtures.projectMetadata
    mempty
    `returnsOnce` ()

uploadSuccessfulAnalysisSpec :: Spec
uploadSuccessfulAnalysisSpec = do
  describe
    "uploadSuccessfulAnalysis"
    $ do
      baseDir <- runIO Fixtures.baseDir
      it' "uploads analysis and git contributors"
        . withGit mockGit
        $ do
          expectGetSuccess
          expectAnalysisUploadSuccess
          expectContributorUploadSuccess
          locator <-
            uploadSuccessfulAnalysis
              baseDir
              Fixtures.projectMetadata
              (toFlag (JsonOutput) False)
              Fixtures.projectRevision
              (SourceUnitOnly Fixtures.sourceUnits)
              mempty
          locator `shouldBe'` expectedLocator
      -- Currently our StdOut logging just writes directly to StdOut, so this is
      -- just checking it doesn't fail.  In the future we should extract that so
      -- we can test it better.
      it' "renders JSON output when requested"
        . withGit mockGit
        $ do
          expectAnalysisUploadSuccess
          expectContributorUploadSuccess
          expectGetSuccess
          locator <-
            uploadSuccessfulAnalysis
              baseDir
              Fixtures.projectMetadata
              (toFlag (JsonOutput) True)
              Fixtures.projectRevision
              (SourceUnitOnly Fixtures.sourceUnits)
              mempty
          locator `shouldBe'` expectedLocator
      it' "performs reachability upload, if organization supports reachability"
        . withGit mockGit
        $ do
          expectGetSuccessWithReachability
          expectAnalysisUploadSuccess
          expectContributorUploadSuccess
          expectReachabilityBuildUpload
          locator <-
            uploadSuccessfulAnalysis
              baseDir
              Fixtures.projectMetadata
              (toFlag (JsonOutput) False)
              Fixtures.projectRevision
              (SourceUnitOnly Fixtures.sourceUnits)
              mempty
          locator `shouldBe'` expectedLocator

      it' "aborts when uploading to a monorepo"
        . expectFatal'
        . withGit mockGit
        $ do
          GetProject Fixtures.projectRevision LocatorTypeCustom `returnsOnce` Fixtures.project{projectIsMonorepo = True}
          uploadSuccessfulAnalysis
            baseDir
            Fixtures.projectMetadata
            (toFlag (JsonOutput) False)
            Fixtures.projectRevision
            (SourceUnitOnly Fixtures.sourceUnits)
            mempty
      it' "continues if fetching the project fails"
        . withGit mockGit
        $ do
          GetProject Fixtures.projectRevision LocatorTypeCustom `fails` "Mocked failure fetching project"
          expectAnalysisUploadSuccess
          expectContributorUploadSuccess
          GetOrganization `alwaysReturns` Fixtures.organization
          GetApiOpts `alwaysReturns` Fixtures.apiOpts

          locator <-
            uploadSuccessfulAnalysis
              baseDir
              Fixtures.projectMetadata
              (toFlag (JsonOutput) False)
              Fixtures.projectRevision
              (SourceUnitOnly Fixtures.sourceUnits)
              mempty
          locator `shouldBe'` expectedLocator
      it' "continues if fetching contributors fails"
        . withGit (\_ -> fatalText "Mocked failure of fetching contributors from git")
        $ do
          expectGetSuccess
          expectAnalysisUploadSuccess
          locator <-
            uploadSuccessfulAnalysis
              baseDir
              Fixtures.projectMetadata
              (toFlag (JsonOutput) False)
              Fixtures.projectRevision
              (SourceUnitOnly Fixtures.sourceUnits)
              mempty
          locator `shouldBe'` expectedLocator
      it' "continues if uploading contributors fails"
        . withGit mockGit
        $ do
          UploadContributors expectedLocator Fixtures.contributors `fails` "Mocked failure uploading contributors"
          expectAnalysisUploadSuccess
          expectGetSuccess
          locator <-
            uploadSuccessfulAnalysis
              baseDir
              Fixtures.projectMetadata
              (toFlag (JsonOutput) False)
              Fixtures.projectRevision
              (SourceUnitOnly Fixtures.sourceUnits)
              mempty
          locator `shouldBe'` expectedLocator
      it' "uploads to S3 and to /api/builds/custom_with_first_party_licenses if there are licenses"
        . withGit mockGit
        $ do
          expectGetSuccess
          expectGetFirstPartySignedUrl PackageRevision{packageName = "testProjectName", packageVersion = "testRevision"}
          expectUploadFirstPartyDataToS3
          expectFirstPartyAnalysisUploadSuccess FileUploadMatchData
          expectContributorUploadSuccess
          locator <-
            uploadSuccessfulAnalysis
              baseDir
              Fixtures.projectMetadata
              (toFlag (JsonOutput) False)
              Fixtures.projectRevision
              (SourceAndLicenseUnits Fixtures.sourceUnits Fixtures.firstLicenseSourceUnit)
              mempty
          locator `shouldBe'` expectedLocator

      it' "uploads to S3 and to /api/builds/custom_with_first_party_licenses if there are licenses and no targets were found"
        . withGit mockGit
        $ do
          expectGetSuccess
          expectGetFirstPartySignedUrl PackageRevision{packageName = "testProjectName", packageVersion = "testRevision"}
          expectUploadFirstPartyDataToS3
          expectFirstPartyAnalysisUploadSuccess FileUploadMatchData
          expectContributorUploadSuccess
          locator <-
            uploadSuccessfulAnalysis
              baseDir
              Fixtures.projectMetadata
              (toFlag (JsonOutput) False)
              Fixtures.projectRevision
              (LicenseSourceUnitOnly Fixtures.firstLicenseSourceUnit)
              mempty
          locator `shouldBe'` expectedLocator

      it' "uploads to S3 and to /api/builds/custom_with_first_party_licenses if there are licenses and full file uploads is set on the org"
        . withGit mockGit
        $ do
          expectGetProject
          expectGetOrganizationWithFullFileUploads
          expectGetApiOpts
          expectGetFirstPartySignedUrl PackageRevision{packageName = "testProjectName", packageVersion = "testRevision"}
          expectUploadFirstPartyDataToS3
          expectFirstPartyAnalysisUploadSuccess FileUploadFullContent
          expectContributorUploadSuccess
          locator <-
            uploadSuccessfulAnalysis
              baseDir
              Fixtures.projectMetadata
              (toFlag (JsonOutput) False)
              Fixtures.projectRevision
              (SourceAndLicenseUnits Fixtures.sourceUnits Fixtures.firstLicenseSourceUnit)
              mempty
          locator `shouldBe'` expectedLocator

mergeSourceAndLicenseUnitsSpec :: Spec
mergeSourceAndLicenseUnitsSpec =
  describe
    "mergeSourceAndLicenseUnits"
    $ do
      it' "merges source and license units" $ do
        let mergedUnits = mergeSourceAndLicenseUnits Fixtures.sourceUnits Fixtures.firstLicenseSourceUnit
        mergedUnits `shouldBe'` expectedMergedFullSourceUnits

spec :: Spec
spec = do
  uploadSuccessfulAnalysisSpec

  mergeSourceAndLicenseUnitsSpec
