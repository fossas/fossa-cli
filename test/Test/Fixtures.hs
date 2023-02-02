{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Fixtures (
  apiOpts,
  baseDir,
  build,
  contributors,
  locator,
  organization,
  packageRevision,
  project,
  projectMetadata,
  projectRevision,
  scanId,
  scanResponse,
  sourceUnits,
  uploadResponse,
  emptyIssues,
  issuesAvailable,
  issuesPending,
  successfulBuild,
  pendingBuild,
  attributionReportAsSerializedJson,
  signedUrl,
  archives,
  firstArchive,
  secondArchive,
  vendoredDeps,
  firstVendoredDep,
  secondVendoredDep,
  locators,
  firstLocator,
  secondLocator,
  firstLicenseSourceUnit,
  secondLicenseSourceUnit,
  diffRevision,
  issuesDiffAvailable,
) where

import App.Fossa.Config.Test (DiffRevision (DiffRevision))
import App.Fossa.VendoredDependency (VendoredDependency (..))
import App.Types qualified as App
import Control.Effect.FossaApiClient qualified as App
import Control.Timeout (Duration (MilliSeconds))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Extra (showT)
import Fossa.API.Types (Archive (..))
import Fossa.API.Types qualified as API
import Path (mkRelDir, parseAbsDir, (</>))
import Srclib.Types (
  LicenseScanType (..),
  LicenseSourceUnit (..),
  Locator (..),
  SourceUnit (..),
  emptyLicenseUnit,
 )
import System.Directory (getTemporaryDirectory)
import Text.URI.QQ (uri)
import Types (ArchiveUploadType (..), GraphBreadth (..))

apiOpts :: API.ApiOpts
apiOpts =
  API.ApiOpts
    { API.apiOptsUri = (Just [uri|https://app.fossa.com/|])
    , API.apiOptsApiKey = API.ApiKey "testApiKey"
    , API.apiOptsPollDelay = MilliSeconds 100
    }

organization :: API.Organization
organization = API.Organization (API.OrgId 42) True True True CLILicenseScan True True True

project :: API.Project
project =
  API.Project
    { API.projectId = "testProjectId"
    , API.projectTitle = "testProjectTitle"
    , API.projectIsMonorepo = False
    }

locator :: Locator
locator =
  Locator
    { locatorFetcher = "testLocator"
    , locatorProject = "testProject"
    , locatorRevision = Just "testRevision"
    }

uploadResponse :: API.UploadResponse
uploadResponse =
  API.UploadResponse
    { API.uploadLocator = locator
    , API.uploadError = Nothing
    }

projectMetadata :: App.ProjectMetadata
projectMetadata =
  App.ProjectMetadata
    { App.projectTitle = Just "testProjectTitle"
    , App.projectUrl = Nothing
    , App.projectJiraKey = Nothing
    , App.projectLink = Nothing
    , App.projectTeam = Nothing
    , App.projectPolicy = Nothing
    , App.projectLabel = []
    , App.projectReleaseGroup = Nothing
    }

projectRevision :: App.ProjectRevision
projectRevision =
  App.ProjectRevision
    { App.projectName = "testProjectName"
    , App.projectRevision = "testRevision"
    , App.projectBranch = Just "testBranch"
    }

diffRevision :: DiffRevision
diffRevision = DiffRevision "someDiffRevision"

packageRevision :: App.PackageRevision
packageRevision =
  App.PackageRevision
    { App.packageName = "testPackageName"
    , App.packageVersion = "1.0"
    }

sourceUnits :: NE.NonEmpty SourceUnit
sourceUnits = NE.fromList [unit]
  where
    unit =
      SourceUnit
        { sourceUnitName = "testSourceUnitName"
        , sourceUnitType = "testSourceUnitType"
        , sourceUnitManifest = "testSourceUnitManifest"
        , sourceUnitBuild = Nothing
        , sourceUnitGraphBreadth = Complete
        , sourceUnitOriginPaths = []
        , additionalData = Nothing
        }

-- | A base dir for testing.  This directory is not guaranteed to exist.  If you
-- want a real directory you should use `withTempDir`.
baseDir :: IO App.BaseDir
baseDir = do
  systemTempDir <- getTemporaryDirectory >>= parseAbsDir
  pure . App.BaseDir $ systemTempDir </> $(mkRelDir "just-a-test-dir")

contributors :: API.Contributors
contributors =
  API.Contributors . Map.fromList $
    [ ("testContributor1", "testContributor1")
    , ("testContributor2", "testContributor2")
    ]

build :: API.Build
build =
  API.Build
    { API.buildId = 101
    , API.buildError = Nothing
    , API.buildTask =
        API.BuildTask
          { API.buildTaskStatus = API.StatusSucceeded
          }
    }

successfulBuild :: API.Build
successfulBuild =
  API.Build
    { API.buildId = 101
    , API.buildError = Nothing
    , API.buildTask =
        API.BuildTask
          { API.buildTaskStatus = API.StatusSucceeded
          }
    }

pendingBuild :: API.Build
pendingBuild =
  API.Build
    { API.buildId = 101
    , API.buildError = Nothing
    , API.buildTask =
        API.BuildTask
          { API.buildTaskStatus = API.StatusRunning
          }
    }

scanId :: API.ScanId
scanId = API.ScanId "TestScanId"

scanResponse :: API.ScanResponse
scanResponse =
  API.ScanResponse
    { API.responseScanId = scanId
    , API.responseScanStatus = Nothing
    }

emptyIssues :: API.Issues
emptyIssues =
  API.Issues
    { API.issuesCount = 0
    , API.issuesIssues = []
    , API.issuesStatus = ""
    , API.issuesSummary = Nothing
    }

issuesAvailable :: API.Issues
issuesAvailable =
  let issueTypes =
        [ API.IssuePolicyConflict
        , API.IssuePolicyFlag
        , API.IssueVulnerability
        , API.IssueUnlicensedDependency
        , API.IssueOutdatedDependency
        , API.IssueOther "TestIssueOther"
        ]
      makeIssue :: Int -> API.IssueType -> API.Issue
      makeIssue issueId issueType =
        API.Issue
          { API.issueId = 200 + issueId
          , API.issuePriorityString = Nothing
          , API.issueResolved = False
          , API.issueRevisionId = "IssueRevisionId" <> showT issueId
          , API.issueType = issueType
          , API.issueRule = Nothing
          , API.issueLicense = Nothing
          }
      issueList = zipWith makeIssue [1 ..] issueTypes
   in API.Issues
        { API.issuesCount = length issueList
        , API.issuesIssues = issueList
        , API.issuesStatus = "SCANNED"
        , API.issuesSummary = Nothing
        }

issuesDiffAvailable :: API.Issues
issuesDiffAvailable =
  let issueTypes =
        [ API.IssuePolicyConflict
        , API.IssuePolicyFlag
        , API.IssueVulnerability
        , API.IssueUnlicensedDependency
        , API.IssueOutdatedDependency
        , API.IssueOther "TestIssueOther"
        ]
      makeIssue :: Int -> API.IssueType -> API.Issue
      makeIssue issueId issueType =
        API.Issue
          { API.issueId = 100 + issueId
          , API.issuePriorityString = Nothing
          , API.issueResolved = False
          , API.issueRevisionId = "IssueRevisionId" <> showT issueId
          , API.issueType = issueType
          , API.issueRule = Nothing
          , API.issueLicense = Nothing
          }
      issueList = zipWith makeIssue [1 ..] issueTypes
   in API.Issues
        { API.issuesCount = length issueList
        , API.issuesIssues = issueList
        , API.issuesStatus = "SCANNED"
        , API.issuesSummary = Nothing
        }

issuesPending :: API.Issues
issuesPending =
  API.Issues
    { API.issuesCount = 0
    , API.issuesIssues = []
    , API.issuesStatus = "WAITING"
    , API.issuesSummary = Nothing
    }

attributionReportAsSerializedJson :: Text
attributionReportAsSerializedJson = "{\"TestReport\": \"TestReportData\"}"

signedUrl :: API.SignedURL
signedUrl = API.SignedURL{API.signedURL = "https://foo.com"}

-- Test data for licenseScanSourceUnits tests
firstVendoredDep :: VendoredDependency
firstVendoredDep =
  VendoredDependency
    "first-archive-test"
    "vendored/foo"
    (Just "0.0.1")

secondVendoredDep :: VendoredDependency
secondVendoredDep =
  VendoredDependency
    "second-archive-test"
    "vendored/bar"
    (Just "0.0.1")

vendoredDeps :: NonEmpty VendoredDependency
vendoredDeps = NE.fromList [firstVendoredDep, secondVendoredDep]

firstLocator :: Locator
firstLocator =
  Locator
    "archive"
    "42/first-archive-test"
    (Just "0.0.1")

secondLocator :: Locator
secondLocator =
  Locator
    "archive"
    "42/second-archive-test"
    (Just "0.0.1")

locators :: NonEmpty Locator
locators = NE.fromList [firstLocator, secondLocator]

firstArchive :: Archive
firstArchive =
  Archive
    "first-archive-test"
    "0.0.1"

secondArchive :: Archive
secondArchive =
  Archive
    "second-archive-test"
    "0.0.1"

archives :: [Archive]
archives = [firstArchive, secondArchive]

firstLicenseSourceUnit :: LicenseSourceUnit
firstLicenseSourceUnit =
  LicenseSourceUnit
    { licenseSourceUnitName = "vendored/foo"
    , licenseSourceUnitType = CliLicenseScanned
    , licenseSourceUnitLicenseUnits = NE.fromList [emptyLicenseUnit]
    }

secondLicenseSourceUnit :: LicenseSourceUnit
secondLicenseSourceUnit =
  LicenseSourceUnit
    { licenseSourceUnitName = "vendored/foo"
    , licenseSourceUnitType = CliLicenseScanned
    , licenseSourceUnitLicenseUnits = NE.fromList [emptyLicenseUnit]
    }
