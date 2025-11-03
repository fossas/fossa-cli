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
  sourceUnitBuildMaven,
  sourceUnitReachabilityNoAnalysis,
  sampleJarParsedContent',
  organizationWithPremiumSubscription,
  pushToken,
  fullAccessToken,
  validCustomUploadPermissions,
  invalidCreateTeamProjectsForReleaseGroupPermission,
  invalidEditReleaseGroupPermission,
  invalidCreateProjectOnlyToTeamPermission,
  invalidCreateTeamProjectPermission,
  invalidEditProjectPermission,
  invalidCreateProjectPermission,
  organizationWithPreflightChecks,
  createReleaseGroupResponse,
  releaseGroup,
  release,
  releaseProject,
  policy,
  team,
  excludePath,
  absFile,
  absDir,
)
where

import App.Fossa.Config.Analyze (AnalysisTacticTypes (Any), AnalyzeConfig (AnalyzeConfig), ExperimentalAnalyzeConfig (..), GoDynamicTactic (..), IncludeAll (..), JsonOutput (JsonOutput), NoDiscoveryExclusion (..), ScanDestination (..), UnpackArchives (..), VSIModeOptions (..), VendoredDependencyOptions (..), WithoutDefaultFilters (..))
import App.Fossa.Config.Analyze qualified as ANZ
import App.Fossa.Config.Analyze qualified as VSI
import App.Fossa.Config.Test (DiffRevision (DiffRevision))
import App.Fossa.Lernie.Types (GrepOptions (..), OrgWideCustomLicenseConfigPolicy (..))
import App.Fossa.Reachability.Types (CallGraphAnalysis (NoCallGraphAnalysis), SourceUnitReachability (..))
import App.Fossa.VSI.Types qualified as VSI
import App.Fossa.VendoredDependency (VendoredDependency (..))
import App.Types (Mode (..), OverrideDynamicAnalysisBinary (..))
import App.Types qualified as App
import Control.Effect.FossaApiClient qualified as App
import Control.Timeout (Duration (MilliSeconds))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LB
import Data.Flag (toFlag)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TL
import Data.Text.Extra (showT)
import Discovery.Filters (
  AllFilters (AllFilters),
  MavenScopeFilters (MavenScopeIncludeFilters),
  comboExclude,
 )
import Fossa.API.CoreTypes qualified as CoreAPI
import Fossa.API.Types (
  Archive (..),
  OrgId (OrgId),
  Organization (..),
  Subscription (..),
 )
import Fossa.API.Types qualified as API
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  mkAbsDir,
  mkRelDir,
  mkRelFile,
  parseAbsDir,
  (</>),
 )
import Srclib.Types (LicenseScanType (..), LicenseSourceUnit (..), Locator (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (..), emptyLicenseUnit)
import System.Directory (getTemporaryDirectory)
import Text.RawString.QQ (r)
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
organization =
  Organization
    { organizationId = (OrgId 42)
    , orgUsesSAML = True
    , orgCoreSupportsLocalLicenseScan = True
    , orgSupportsAnalyzedRevisionsQuery = True
    , orgDefaultVendoredDependencyScanType = CLILicenseScan
    , orgSupportsIssueDiffs = True
    , orgSupportsNativeContainerScan = True
    , orgSupportsDependenciesCachePolling = True
    , orgRequiresFullFileUploads = False
    , orgDefaultsToFirstPartyScans = False
    , orgSupportsPathDependencyScans = False
    , orgSupportsFirstPartyScans = True
    , orgCustomLicenseScanConfigs = []
    , orgSupportsReachability = False
    , orgSupportsPreflightChecks = False
    , orgSubscription = Free
    , orgSnippetScanSourceCodeRetentionDays = Nothing
    }

organizationWithPreflightChecks :: API.Organization
organizationWithPreflightChecks =
  Organization
    { organizationId = (OrgId 42)
    , orgUsesSAML = True
    , orgCoreSupportsLocalLicenseScan = True
    , orgSupportsAnalyzedRevisionsQuery = True
    , orgDefaultVendoredDependencyScanType = CLILicenseScan
    , orgSupportsIssueDiffs = True
    , orgSupportsNativeContainerScan = True
    , orgSupportsDependenciesCachePolling = True
    , orgRequiresFullFileUploads = False
    , orgDefaultsToFirstPartyScans = False
    , orgSupportsPathDependencyScans = False
    , orgSupportsFirstPartyScans = True
    , orgCustomLicenseScanConfigs = []
    , orgSupportsReachability = False
    , orgSupportsPreflightChecks = True
    , orgSubscription = Free
    , orgSnippetScanSourceCodeRetentionDays = Nothing
    }

organizationWithPremiumSubscription :: API.Organization
organizationWithPremiumSubscription =
  Organization
    { organizationId = (OrgId 42)
    , orgUsesSAML = True
    , orgCoreSupportsLocalLicenseScan = True
    , orgSupportsAnalyzedRevisionsQuery = True
    , orgDefaultVendoredDependencyScanType = CLILicenseScan
    , orgSupportsIssueDiffs = True
    , orgSupportsNativeContainerScan = True
    , orgSupportsDependenciesCachePolling = True
    , orgRequiresFullFileUploads = False
    , orgDefaultsToFirstPartyScans = False
    , orgSupportsPathDependencyScans = False
    , orgSupportsFirstPartyScans = True
    , orgCustomLicenseScanConfigs = []
    , orgSupportsReachability = False
    , orgSupportsPreflightChecks = True
    , orgSubscription = Premium
    , orgSnippetScanSourceCodeRetentionDays = Nothing
    }

pushToken :: API.TokenTypeResponse
pushToken = API.TokenTypeResponse API.Push

fullAccessToken :: API.TokenTypeResponse
fullAccessToken = API.TokenTypeResponse API.FullAccess

invalidCreateProjectPermission :: API.CustomBuildUploadPermissions
invalidCreateProjectPermission = API.CustomBuildUploadPermissions API.InvalidCreateProjectPermission Nothing

invalidEditProjectPermission :: API.CustomBuildUploadPermissions
invalidEditProjectPermission = API.CustomBuildUploadPermissions API.InvalidEditProjectPermission Nothing

invalidCreateTeamProjectPermission :: API.CustomBuildUploadPermissions
invalidCreateTeamProjectPermission = API.CustomBuildUploadPermissions API.InvalidCreateTeamProjectPermission Nothing

invalidCreateProjectOnlyToTeamPermission :: API.CustomBuildUploadPermissions
invalidCreateProjectOnlyToTeamPermission = API.CustomBuildUploadPermissions API.InvalidCreateProjectOnlyToTeamPermission Nothing

invalidEditReleaseGroupPermission :: API.CustomBuildUploadPermissions
invalidEditReleaseGroupPermission = API.CustomBuildUploadPermissions API.ValidProjectPermission $ Just API.InvalidEditReleaseGroupPermission

invalidCreateTeamProjectsForReleaseGroupPermission :: API.CustomBuildUploadPermissions
invalidCreateTeamProjectsForReleaseGroupPermission = API.CustomBuildUploadPermissions API.ValidProjectPermission $ Just API.InvalidCreateTeamProjectsForReleaseGroupPermission

validCustomUploadPermissions :: API.CustomBuildUploadPermissions
validCustomUploadPermissions = API.CustomBuildUploadPermissions API.ValidProjectPermission $ Just API.ValidReleaseGroupPermission

createReleaseGroupResponse :: CoreAPI.CreateReleaseGroupResponse
createReleaseGroupResponse = CoreAPI.CreateReleaseGroupResponse 1

releaseGroup :: CoreAPI.ReleaseGroup
releaseGroup =
  CoreAPI.ReleaseGroup
    { CoreAPI.releaseGroupId = 1
    , CoreAPI.releaseGroupTitle = "example-title"
    , CoreAPI.releaseGroupReleases = [release]
    }

release :: CoreAPI.ReleaseGroupRelease
release =
  CoreAPI.ReleaseGroupRelease
    { CoreAPI.releaseGroupReleaseId = 2
    , CoreAPI.releaseGroupReleaseTitle = "example-release-title"
    , CoreAPI.releaseGroupReleaseProjects = [releaseProject]
    }

releaseProject :: CoreAPI.ReleaseProject
releaseProject =
  CoreAPI.ReleaseProject
    { CoreAPI.releaseProjectLocator = "custom+1/example"
    , CoreAPI.releaseProjectRevisionId = "custom+1/example$123"
    , CoreAPI.releaseProjectBranch = "main"
    }

policy :: CoreAPI.Policy
policy =
  CoreAPI.Policy
    { CoreAPI.policyId = 7
    , CoreAPI.policyTitle = "example-policy"
    , CoreAPI.policyType = CoreAPI.LICENSING
    }

team :: CoreAPI.Team
team =
  CoreAPI.Team
    { CoreAPI.teamId = 10
    , CoreAPI.teamName = "example-team"
    }

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
    , API.uploadWarnings = Nothing
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

sourceUnits :: [SourceUnit]
sourceUnits = [unit]
  where
    unit =
      SourceUnit
        { sourceUnitName = "testSourceUnitName"
        , sourceUnitType = "testSourceUnitType"
        , sourceUnitManifest = "testSourceUnitManifest"
        , sourceUnitBuild = Nothing
        , sourceUnitGraphBreadth = Complete
        , sourceUnitOriginPaths = []
        , sourceUnitNoticeFiles = []
        , additionalData = Nothing
        , sourceUnitLabels = Nothing
        }

sourceUnitBuildMaven :: SourceUnitBuild
sourceUnitBuildMaven =
  SourceUnitBuild
    "default"
    True
    [ ipAddr
    , spotBugs
    ]
    [ SourceUnitDependency logger [] Aeson.Null
    , SourceUnitDependency ipAddr [] Aeson.Null
    , SourceUnitDependency
        spotBugs
        [ logger
        ]
        Aeson.Null
    ]
  where
    ipAddr :: Locator
    ipAddr = mkLocator "com.github.seancfoley:ipaddress" "5.4.0"

    spotBugs :: Locator
    spotBugs = mkLocator "com.github.spotbugs:spotbugs-annotations" "4.8.3"

    logger :: Locator
    logger = mkLocator "org.apache.logging.log4j:log4j-core" "2.22.1"

    mkLocator :: Text -> Text -> Locator
    mkLocator name version = Locator "mvn" name (Just version)

sourceUnitReachabilityNoAnalysis :: SourceUnitReachability
sourceUnitReachabilityNoAnalysis = SourceUnitReachability "type" "manifest" "name" [] [] NoCallGraphAnalysis

vsiSourceUnit :: SourceUnit
vsiSourceUnit =
  SourceUnit
    { sourceUnitName = "/tmp/one/two"
    , sourceUnitType = "vsi"
    , sourceUnitManifest = "/tmp/one/two"
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
                    , sourceDepData = Aeson.Null
                    }
                ]
            }
    , sourceUnitGraphBreadth = Complete
    , sourceUnitOriginPaths = ["/tmp/one/two"]
    , sourceUnitNoticeFiles = []
    , additionalData = Nothing
    , sourceUnitLabels = Nothing
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
    Nothing
    []

secondVendoredDep :: VendoredDependency
secondVendoredDep =
  VendoredDependency
    "second-archive-test"
    "vendored/bar"
    (Just "0.0.1")
    Nothing
    []

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
    Nothing
    Nothing

secondArchive :: Archive
secondArchive =
  Archive
    "second-archive-test"
    "0.0.1"
    Nothing
    Nothing

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
    , vsiSkipSet = VSI.SkipResolution Set.empty
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
    , resolvePathDependencies = False
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

customFossaDepsFile :: Maybe FilePath
customFossaDepsFile = Nothing

mavenScopeFilterSet :: MavenScopeFilters
mavenScopeFilterSet = MavenScopeIncludeFilters mempty

#ifdef mingw32_HOST_OS
-- | Arbitrary absolute directory path for tests that require one.
absDir :: Path Abs Dir
absDir = $(mkAbsDir "C:/")
#else
absDir :: Path Abs Dir
absDir = $(mkAbsDir "/")
#endif

-- | Arbitrary absolute file path for tests that require one.
absFile :: Path Abs File
absFile = absDir </> $(Path.mkRelFile "file.txt")

standardAnalyzeConfig :: AnalyzeConfig
standardAnalyzeConfig =
  AnalyzeConfig
    { ANZ.baseDir = App.BaseDir absDir
    , ANZ.scanDestination = OutputStdout
    , ANZ.projectRevision = projectRevision
    , ANZ.vsiOptions = vsiOptions
    , ANZ.filterSet = filterSet
    , ANZ.mavenScopeFilterSet = mavenScopeFilterSet
    , ANZ.experimental = experimentalConfig
    , ANZ.vendoredDeps = vendoredDepsOptions
    , ANZ.unpackArchives = toFlag UnpackArchives False
    , ANZ.jsonOutput = toFlag JsonOutput False
    , ANZ.includeAllDeps = toFlag IncludeAll False
    , ANZ.noDiscoveryExclusion = toFlag NoDiscoveryExclusion False
    , ANZ.overrideDynamicAnalysis = App.OverrideDynamicAnalysisBinary{unOverrideDynamicAnalysisBinary = mempty}
    , ANZ.firstPartyScansFlag = App.FirstPartyScansUseDefault
    , ANZ.grepOptions = grepOptions
    , ANZ.customFossaDepsFile = customFossaDepsFile
    , ANZ.allowedTacticTypes = Any
    , ANZ.reachabilityConfig = mempty
    , ANZ.withoutDefaultFilters = toFlag WithoutDefaultFilters False
    , ANZ.mode = NonStrict
    , ANZ.xSnippetScan = False
    , ANZ.debugDir = Nothing
    , ANZ.xVendetta = False
    }

sampleJarParsedContent :: Text
sampleJarParsedContent =
  [r|C:vuln.project.sample.App java.lang.Object
C:vuln.project.sample.App java.net.URI
C:vuln.project.sample.App java.lang.System
C:vuln.project.sample.App vuln.project.sample.App
C:vuln.project.sample.App java.io.PrintStream
C:vuln.project.sample.App org.dom4j.io.SAXReader
C:vuln.project.sample.App java.lang.Exception
C:vuln.project.sample.App org.dom4j.DocumentException
M:vuln.project.sample.App:<init>() (O)java.lang.Object:<init>()
M:vuln.project.sample.App:main(java.lang.String[]) (O)java.net.URI:<init>(java.lang.String)
M:vuln.project.sample.App:main(java.lang.String[]) (M)java.net.URI:toURL()
M:vuln.project.sample.App:main(java.lang.String[]) (S)vuln.project.sample.App:parse(java.net.URL)
M:vuln.project.sample.App:main(java.lang.String[]) (M)java.io.PrintStream:println(java.lang.Object)
M:vuln.project.sample.App:parse(java.net.URL) (O)org.dom4j.io.SAXReader:<init>()
M:vuln.project.sample.App:parse(java.net.URL) (M)org.dom4j.io.SAXReader:read(java.net.URL)|]

sampleJarParsedContent' :: LB.ByteString
sampleJarParsedContent' = LB.fromStrict . TL.encodeUtf8 $ sampleJarParsedContent

excludePath :: Path Rel Dir -> AllFilters
excludePath path = AllFilters mempty $ comboExclude mempty [path]
