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
  firstLicenseSourceUnit,
  secondLicenseSourceUnit,
) where

import App.Types qualified as App
import Control.Effect.FossaApiClient qualified as App
import Control.Timeout (Duration (MilliSeconds))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Extra (showT)
import Fossa.API.Types qualified as API
import Path (mkRelDir, parseAbsDir, (</>))
import Srclib.Types (LicenseScanType (..), LicenseSourceUnit (..), LicenseUnit (..), LicenseUnitData (..), LicenseUnitInfo (..), LicenseUnitMatchData (..), Locator (..), SourceUnit (..))
import System.Directory (getTemporaryDirectory)
import Text.URI.QQ (uri)
import Types (GraphBreadth (..))

apiOpts :: API.ApiOpts
apiOpts =
  API.ApiOpts
    { API.apiOptsUri = (Just [uri|https://app.fossa.com/|])
    , API.apiOptsApiKey = API.ApiKey "testApiKey"
    , API.apiOptsPollDelay = MilliSeconds 100
    }

organization :: API.Organization
organization = API.Organization (API.OrgId 42) True False

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
    , App.projectReleaseGroup = Nothing
    }

projectRevision :: App.ProjectRevision
projectRevision =
  App.ProjectRevision
    { App.projectName = "testProjectName"
    , App.projectRevision = "testRevision"
    , App.projectBranch = Just "testBranch"
    }

packageRevision :: App.PackageRevision
packageRevision =
  App.PackageRevision
    { App.packageName = "testPackageName"
    , App.packageVersion = "1.0"
    }

sourceUnits :: NE.NonEmpty SourceUnit
sourceUnits = unit NE.:| []
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
          }
      issueList = zipWith makeIssue [1 ..] issueTypes
   in API.Issues
        { API.issuesCount = length issueList
        , API.issuesIssues = issueList
        , API.issuesStatus = "SCANNED"
        }

issuesPending :: API.Issues
issuesPending =
  API.Issues
    { API.issuesCount = 0
    , API.issuesIssues = []
    , API.issuesStatus = "WAITING"
    }

attributionReportAsSerializedJson :: Text
attributionReportAsSerializedJson = "{\"TestReport\": \"TestReportData\"}"

signedUrl :: API.SignedURL
signedUrl = API.SignedURL{API.signedURL = "https://foo.com"}

firstLicenseSourceUnit :: LicenseSourceUnit
firstLicenseSourceUnit = LicenseSourceUnit{licenseSourceUnitName = "vendor/foo", licenseSourceUnitType = CliLicenseScanned, licenseSourceUnitLicenseUnits = LicenseUnit{licenseUnitName = "bsd-new", licenseUnitType = "LicenseUnit", licenseUnitDir = "", licenseUnitFiles = "vendor/foo/BSD_NEW_LICENSE" NE.:| [], licenseUnitData = LicenseUnitData{licenseUnitDataPath = "vendor/foo/BSD_NEW_LICENSE", licenseUnitDataCopyright = Nothing, licenseUnitDataThemisVersion = "3332fcd9e1bd34219e4d8452f72df0a38cb4d0bc", licenseUnitDataMatchData = Just (LicenseUnitMatchData{licenseUnitMatchDataMatchString = "Redistribution and use in source and binary forms, with or without modification,\nare permitted provided that the following conditions are met:\n\nRedistributions of source code must retain the above copyright notice, this list\nof conditions and the following disclaimer.\n\nRedistributions in binary form must reproduce the above copyright notice, this\nlist of conditions and the following disclaimer in the documentation and/or\nother materials provided with the distribution.\n\nNeither the name of the ORGANIZATION nor the names of its contributors may be\nused to endorse or promote products derived from this software without specific\nprior written permission.\n\nTHIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n\"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,\nTHE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\nARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS\nBE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\nCONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE\nGOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)\nHOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\nLIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF\nTHE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.", licenseUnitMatchDataLocation = 0, licenseUnitMatchDataLength = 1413, licenseUnitMatchDataIndex = 0, licenseUnitDataStartLine = 1, licenseUnitDataEndLine = 24} NE.:| []), licenseUnitDataCopyrights = Nothing} NE.:| [], licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}} NE.:| []}

secondLicenseSourceUnit :: LicenseSourceUnit
secondLicenseSourceUnit = LicenseSourceUnit{licenseSourceUnitName = "vendor/bar", licenseSourceUnitType = CliLicenseScanned, licenseSourceUnitLicenseUnits = LicenseUnit{licenseUnitName = "mit", licenseUnitType = "LicenseUnit", licenseUnitDir = "", licenseUnitFiles = "vendor/bar/MIT_LICENSE" NE.:| [], licenseUnitData = LicenseUnitData{licenseUnitDataPath = "vendor/bar/MIT_LICENSE", licenseUnitDataCopyright = Nothing, licenseUnitDataThemisVersion = "3332fcd9e1bd34219e4d8452f72df0a38cb4d0bc", licenseUnitDataMatchData = Just (LicenseUnitMatchData{licenseUnitMatchDataMatchString = "Permission is hereby granted, free of charge, to any person obtaining\na copy of this software and associated documentation files (the\n\"Software\"), to deal in the Software without restriction, including\nwithout limitation the rights to use, copy, modify, merge, publish,\ndistribute, sublicense, and/or sell copies of the Software, and to\npermit persons to whom the Software is furnished to do so, subject to\nthe following conditions:\n\nThe above copyright notice and this permission notice shall be\nincluded in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\nEXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\nMERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.\nIN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY\nCLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,\nTORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE\nSOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.", licenseUnitMatchDataLocation = 0, licenseUnitMatchDataLength = 1021, licenseUnitMatchDataIndex = 0, licenseUnitDataStartLine = 1, licenseUnitDataEndLine = 18} NE.:| []), licenseUnitDataCopyrights = Nothing} NE.:| [], licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}} NE.:| []}
