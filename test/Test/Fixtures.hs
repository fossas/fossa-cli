{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Fixtures (
  apiOpts,
  baseDir,
  build,
  contributors,
  locator,
  organization,
  project,
  projectMetadata,
  projectRevision,
  scanId,
  scanResponse,
  sourceUnits,
  uploadResponse,
  emptyIssues,
  issues,
) where

import App.Types qualified as App
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text.Extra (showT)
import Fossa.API.Types qualified as API
import Path (mkRelDir, parseAbsDir, (</>))
import Srclib.Types (Locator (..), SourceUnit (..))
import System.Directory (getTemporaryDirectory)
import Text.URI.QQ (uri)
import Types (GraphBreadth (..))

apiOpts :: API.ApiOpts
apiOpts =
  API.ApiOpts (Just [uri|https://app.fossa.com/|]) $ API.ApiKey "testApiKey"

organization :: API.Organization
organization = API.Organization 42 True False

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

issues :: API.Issues
issues =
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
        , API.issuesStatus = "AVAILABLE"
        }
