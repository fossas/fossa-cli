{-# LANGUAGE CPP #-}
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
  standardAnalyzeConfig,
  vsiSourceUnit,
) where

import App.Fossa.Config.Analyze (AnalyzeConfig (AnalyzeConfig), ExperimentalAnalyzeConfig (..), GoDynamicTactic (..), IncludeAll (..), JsonOutput (JsonOutput), NoDiscoveryExclusion (..), ScanDestination (..), UnpackArchives (..), VSIModeOptions (..), VendoredDependencyOptions (..))
import App.Fossa.Config.Analyze qualified as ANZ
import App.Fossa.Config.Analyze qualified as VSI
import App.Fossa.Config.Test (DiffRevision (DiffRevision))
import App.Fossa.Lernie.Types (GrepOptions (..), OrgWideCustomLicenseConfigPolicy (..))
import App.Fossa.VSI.Types qualified as VSI
import App.Fossa.VendoredDependency (VendoredDependency (..))
import App.Types (OverrideDynamicAnalysisBinary (..))
import App.Types qualified as App
import Control.Effect.FossaApiClient qualified as App
import Control.Monad.RWS qualified as Set
import Control.Timeout (Duration (MilliSeconds))
import Data.Flag (toFlag)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Data.Text.Extra (showT)
import Discovery.Filters (
  AllFilters,
 )
import Effect.Logger (Severity (..))
import Fossa.API.Types (Archive (..))
import Fossa.API.Types qualified as API
import Path (Abs, Dir, Path, mkAbsDir, mkRelDir, parseAbsDir, (</>))
import Srclib.Types (LicenseScanType (..), LicenseSourceUnit (..), Locator (..), OriginPath, SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (..), emptyLicenseUnit)
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
organization = API.Organization (API.OrgId 42) True True True CLILicenseScan True True True False False True []

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
    , App.projectLabel = ["label-1", "label-2"]
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

vsiOriginPath :: OriginPath
#ifdef mingw32_HOST_OS
vsiOriginPath = "D:\\tmp\\one\\two\\"
#else
vsiOriginPath = "/tmp/one/two/"
#endif

vsiSourceUnit :: SourceUnit
vsiSourceUnit =
  SourceUnit
    { sourceUnitName = toText vsiOriginPath
    , sourceUnitType = "vsi"
    , sourceUnitManifest = toText vsiOriginPath
    , sourceUnitBuild =
        Just
          SourceUnitBuild
            { buildArtifact = "default"
            , buildSucceeded = True
            , buildImports =
                [ Locator
                    { locatorFetcher = "mvn"
                    , locatorProject = "something/mavenish"
                    , locatorRevision = Just "1.2.3"
                    }
                ]
            , buildDependencies =
                [ SourceUnitDependency
                    { sourceDepLocator =
                        Locator
                          { locatorFetcher = "mvn"
                          , locatorProject = "something/mavenish"
                          , locatorRevision = Just "1.2.3"
                          }
                    , sourceDepImports = []
                    }
                ]
            }
    , sourceUnitGraphBreadth = Complete
    , sourceUnitOriginPaths = [vsiOriginPath]
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

emptyIssues :: API.Issues
emptyIssues =
  API.Issues
    { API.issuesCount = 0
    , API.issuesIssues = []
    , API.issuesStatus = ""
    , API.issuesSummary = Nothing
    }

makeIssue :: Int -> API.IssueType -> API.Issue
makeIssue issueId issueType =
  API.Issue
    { API.issueId = issueId
    , API.issuePriorityString = Nothing
    , API.issueResolved = False
    , API.issueRevisionId = "IssueRevisionId" <> showT issueId
    , API.issueType = issueType
    , API.issueRule = Nothing
    , API.issueLicense = Nothing
    , API.issueDashURL = Nothing
    , API.issueCVE = Nothing
    , API.issueFixedIn = Nothing
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
      issueList = zipWith makeIssue [201 ..] issueTypes
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
      issueList = zipWith makeIssue [101 ..] issueTypes
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

vsiOptions :: VSI.VSIModeOptions
vsiOptions =
  VSI.VSIModeOptions
    { vsiAnalysisEnabled = toFlag VSI.VSIAnalysis False
    , vsiSkipSet = VSI.SkipResolution Set.mempty
    , iatAssertion = VSI.IATAssertion Nothing
    , dynamicLinkingTarget = VSI.DynamicLinkInspect Nothing
    , binaryDiscoveryEnabled = toFlag VSI.BinaryDiscovery False
    }

filterSet :: AllFilters
filterSet = mempty

experimentalConfig :: ExperimentalAnalyzeConfig
experimentalConfig =
  ExperimentalAnalyzeConfig
    { allowedGradleConfigs = Nothing
    , useV3GoResolver = GoModulesBasedTactic
    }

vendoredDepsOptions :: VendoredDependencyOptions
vendoredDepsOptions =
  VendoredDependencyOptions
    { forceRescans = False
    , licenseScanMethod = Just CLILicenseScan
    , licenseScanPathFilters = Nothing
    }

grepOptions :: GrepOptions
grepOptions =
  GrepOptions
    { customLicenseSearch = []
    , keywordSearch = []
    , orgWideCustomLicenseScanConfigPolicy = Use
    , configFilePath = Nothing
    }

#ifdef mingw32_HOST_OS
absDir :: Path Abs Dir
absDir = $(mkAbsDir "C:/")
#else
absDir :: Path Abs Dir
absDir = $(mkAbsDir "/")
#endif

standardAnalyzeConfig :: AnalyzeConfig
standardAnalyzeConfig =
  AnalyzeConfig
    { ANZ.baseDir = App.BaseDir absDir
    , ANZ.severity = SevDebug
    , ANZ.scanDestination = OutputStdout
    , ANZ.projectRevision = projectRevision
    , ANZ.vsiOptions = vsiOptions
    , ANZ.filterSet = filterSet
    , ANZ.experimental = experimentalConfig
    , ANZ.vendoredDeps = vendoredDepsOptions
    , ANZ.unpackArchives = toFlag UnpackArchives False
    , ANZ.jsonOutput = toFlag JsonOutput False
    , ANZ.includeAllDeps = toFlag IncludeAll False
    , ANZ.noDiscoveryExclusion = toFlag NoDiscoveryExclusion False
    , ANZ.overrideDynamicAnalysis = App.OverrideDynamicAnalysisBinary{unOverrideDynamicAnalysisBinary = mempty}
    , ANZ.firstPartyScansFlag = App.FirstPartyScansUseDefault
    , ANZ.grepOptions = grepOptions
    }
