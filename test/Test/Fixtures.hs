{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Fixtures (apiOpts, organization, sourceUnits, project, uploadResponse, projectMedata, Test.Fixtures.projectRevision, baseDir, contributors) where

import App.Types (BaseDir (BaseDir), ProjectMetadata (..), ProjectRevision (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Fossa.API.Types (
  ApiKey (ApiKey),
  ApiOpts (ApiOpts),
  Contributors (Contributors),
  Organization (Organization),
  Project (..),
  UploadResponse (..),
 )
import Path (mkAbsDir)
import Srclib.Types (SourceUnit (..))
import Text.URI.QQ (uri)
import Types (GraphBreadth (..))

apiOpts :: ApiOpts
apiOpts =
  ApiOpts (Just [uri|https://app.fossa.com/|]) $ ApiKey "apiKey"

organization :: Organization
organization = Organization 42 True False

project :: Project
project =
  Project
    { projectId = "projectId"
    , Fossa.API.Types.projectTitle = "projectTitle"
    , projectIsMonorepo = False
    }

uploadResponse :: UploadResponse
uploadResponse =
  UploadResponse
    { uploadLocator = "locator+project$revision"
    , uploadError = Nothing
    }

projectMedata :: ProjectMetadata
projectMedata =
  ProjectMetadata
    { App.Types.projectTitle = Just "project"
    , projectUrl = Nothing
    , projectJiraKey = Nothing
    , projectLink = Nothing
    , projectTeam = Nothing
    , projectPolicy = Nothing
    , projectReleaseGroup = Nothing
    }

projectRevision :: ProjectRevision
projectRevision =
  ProjectRevision
    { projectName = "project"
    , App.Types.projectRevision = "revision"
    , projectBranch = Just "main"
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

baseDir :: BaseDir
baseDir = BaseDir $(mkAbsDir "/not-a-real-dir")

contributors :: Contributors
contributors =
  Contributors . Map.fromList $
    [ ("Contributor1", "Contributor1")
    , ("Contributor2", "Contributor2")
    ]
