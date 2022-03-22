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
import Srclib.Types (SourceUnit (..))
import Text.URI.QQ (uri)
import Types (GraphBreadth (..))

apiOpts :: API.ApiOpts
apiOpts =
  API.ApiOpts (Just [uri|https://app.fossa.com/|]) $ API.ApiKey "apiKey"

organization :: API.Organization
organization = API.Organization 42 True False

project :: API.Project
project =
  API.Project
    { API.projectId = "projectId"
    , API.projectTitle = "projectTitle"
    , API.projectIsMonorepo = False
    }

uploadResponse :: API.UploadResponse
uploadResponse =
  API.UploadResponse
    { API.uploadLocator = "locator+project$revision"
    , API.uploadError = Nothing
    }

projectMedata :: App.ProjectMetadata
projectMedata =
  App.ProjectMetadata
    { App.projectTitle = Just "project"
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
    { App.projectName = "project"
    , App.projectRevision = "revision"
    , App.projectBranch = Just "main"
    }

sourceUnits :: NE.NonEmpty SourceUnit
sourceUnits =
  NE.fromList
    [ SourceUnit
        { sourceUnitName = "sourceUnit"
        , sourceUnitType = "sourceUnitType"
        , sourceUnitManifest = "sourceUnitManifest"
        , sourceUnitBuild = Nothing
        , sourceUnitGraphBreadth = Complete
        , sourceUnitOriginPaths = []
        , additionalData = Nothing
        }
    ]

baseDir :: App.BaseDir
baseDir = App.BaseDir $(mkAbsDir "/not-a-real-dir")

contributors :: API.Contributors
contributors =
  API.Contributors . Map.fromList $
    [ ("Contributor1", "Contributor1")
    , ("Contributor2", "Contributor2")
    ]
