{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Fixtures (
  apiOpts,
  organization,
  sourceUnits,
  project,
  uploadResponse,
  projectMedata,
  projectRevision,
  baseDir,
  contributors,
) where

import App.Types qualified as App
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Fossa.API.Types qualified as API
import Path (mkAbsDir)
import Srclib.Types (Locator (..), SourceUnit (..))
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

projectMedata :: App.ProjectMetadata
projectMedata =
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
sourceUnits =
  NE.fromList
    [ SourceUnit
        { sourceUnitName = "testSourceUnitName"
        , sourceUnitType = "testSourceUnitType"
        , sourceUnitManifest = "testSourceUnitManifest"
        , sourceUnitBuild = Nothing
        , sourceUnitGraphBreadth = Complete
        , sourceUnitOriginPaths = []
        , additionalData = Nothing
        }
    ]

baseDir :: App.BaseDir
baseDir = App.BaseDir $(mkAbsDir "/tmp/just-a-test-dir")

contributors :: API.Contributors
contributors =
  API.Contributors . Map.fromList $
    [ ("testContributor1", "testContributor1")
    , ("testContributor2", "testContributor2")
    ]
