{-# LANGUAGE RecordWildCards #-}

module Fossa.API.CoreTypes (
  Project (..),
  Policy (..),
  Team (..),
  PolicyType (..),
  ReleaseGroup (..),
  ReleaseGroupRelease (..),
  ReleaseProject (..),
  UpdateReleaseProjectRequest (..),
  UpdateReleaseRequest (..),
  CreateReleaseGroupRequest (..),
  CreateReleaseGroupResponse (..),
  UpdateProjectRequest (..),
  AddTeamProjectsRequest (..),
  AddTeamProjectsResponse (..),
  TeamProjectAction (..),
  UpdateRevisionRequest (..),
  Label (..),
  Labels (..),
  Revision (..),
) where

import App.Types (ReleaseGroupReleaseRevision)
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  Value (Null),
  object,
  withObject,
  withText,
  (.:),
 )
import Data.Text (Text)

--- | Data types of Core's main endpoints

-- Policies
data Policy = Policy
  { policyId :: Int
  , policyTitle :: Text
  , policyType :: PolicyType
  }
  deriving (Eq, Ord, Show)

instance FromJSON Policy where
  parseJSON = withObject "Policy" $ \obj ->
    Policy
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "type"

instance FromJSON PolicyType where
  parseJSON = withText "PolicyType" $ \case
    "LICENSING" -> pure LICENSING
    "SECURITY" -> pure SECURITY
    "QUALITY" -> pure QUALITY
    other -> pure $ PolicyUnknown other

data PolicyType
  = LICENSING
  | SECURITY
  | QUALITY
  | PolicyUnknown Text
  deriving (Eq, Ord, Show)

-- Teams
data Team = Team
  { teamId :: Int
  , teamName :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON Team where
  parseJSON = withObject "Team" $ \obj ->
    Team
      <$> obj .: "id"
      <*> obj .: "name"

data TeamProjectAction = Add
  deriving (Eq, Ord, Show)

instance ToJSON TeamProjectAction where
  toJSON Add = "add"

data AddTeamProjectsRequest = AddTeamProjectsRequest
  { projectLocators :: [Text]
  , action :: TeamProjectAction
  }
  deriving (Eq, Ord, Show)

instance ToJSON AddTeamProjectsRequest where
  toJSON AddTeamProjectsRequest{..} =
    object
      [ "projects" .= projectLocators
      , "action" .= action
      ]

newtype AddTeamProjectsResponse = AddTeamProjectsResponse {teamProjectLocators :: [Text]}
  deriving (Eq, Ord, Show)

instance FromJSON AddTeamProjectsResponse where
  parseJSON = withObject "AddTeamProjectsResponse" $ \obj ->
    AddTeamProjectsResponse
      <$> obj .: "projects"

-- ReleaseGroup
data ReleaseGroup = ReleaseGroup
  { releaseGroupId :: Int
  , releaseGroupTitle :: Text
  , releaseGroupReleases :: [ReleaseGroupRelease]
  }
  deriving (Eq, Ord, Show)

instance FromJSON ReleaseGroup where
  parseJSON = withObject "ReleaseGroup" $ \obj ->
    ReleaseGroup
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "releases"

data ReleaseGroupRelease = ReleaseGroupRelease
  { releaseGroupReleaseId :: Int
  , releaseGroupReleaseTitle :: Text
  , releaseGroupReleaseProjects :: [ReleaseProject]
  }
  deriving (Eq, Ord, Show)

instance FromJSON ReleaseGroupRelease where
  parseJSON = withObject "ReleaseGroupRelease" $ \obj ->
    ReleaseGroupRelease
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "projects"

data ReleaseProject = ReleaseProject
  { releaseProjectLocator :: Text
  , releaseProjectRevisionId :: Text
  , releaseProjectBranch :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ReleaseProject where
  parseJSON = withObject "ReleaseProject" $ \obj ->
    ReleaseProject
      <$> obj .: "projectId"
      <*> obj .: "revisionId"
      <*> obj .: "branch"

data UpdateReleaseRequest = UpdateReleaseRequest
  { updatedReleaseTitle :: Text
  , updatedProjects :: [UpdateReleaseProjectRequest]
  }
  deriving (Eq, Ord, Show)

instance ToJSON UpdateReleaseRequest where
  toJSON UpdateReleaseRequest{..} =
    object
      [ "title" .= updatedReleaseTitle
      , "projects" .= updatedProjects
      ]

data UpdateReleaseProjectRequest = UpdateReleaseProjectRequest
  { updateReleaseProjectLocator :: Text
  , updateReleaseProjectRevisionId :: Text
  , updateReleaseProjectBranch :: Text
  , -- Existence of this field signifies to core that the project exists and the release project just needs to be updated.
    -- If this field is empty it means that we are adding a new project to the release.
    targetReleaseGroupId :: Maybe Int
  }
  deriving (Eq, Ord, Show)

instance ToJSON UpdateReleaseProjectRequest where
  toJSON UpdateReleaseProjectRequest{..} =
    object
      [ "projectId" .= updateReleaseProjectLocator
      , "revisionId" .= updateReleaseProjectRevisionId
      , "branch" .= updateReleaseProjectBranch
      , "projectGroupReleaseId" .= maybe Null toJSON targetReleaseGroupId
      ]

data CreateReleaseGroupRequest = CreateReleaseGroupRequest
  { title :: Text
  , release :: ReleaseGroupReleaseRevision
  , maybeLicensePolicyId :: Maybe Int
  , maybeSecurityPolicyId :: Maybe Int
  , maybeQualityPolicyId :: Maybe Int
  , maybeTeamIds :: Maybe [Int]
  }
  deriving (Eq, Ord, Show)

instance ToJSON CreateReleaseGroupRequest where
  toJSON CreateReleaseGroupRequest{..} =
    object
      [ "title" .= title
      , "release" .= release
      , "licensingPolicyId" .= maybe Null toJSON maybeLicensePolicyId
      , "securityPolicyId" .= maybe Null toJSON maybeSecurityPolicyId
      , "qualityPolicyId" .= maybe Null toJSON maybeQualityPolicyId
      , "teams" .= maybe Null toJSON maybeTeamIds
      ]

newtype CreateReleaseGroupResponse = CreateReleaseGroupResponse {groupId :: Int}
  deriving (Eq, Ord, Show)

instance FromJSON CreateReleaseGroupResponse where
  parseJSON = withObject "CreateReleaseGroupResponse" $ \obj ->
    CreateReleaseGroupResponse
      <$> obj .: "id"

-- Project
data Project = Project
  { projectLocator :: Text
  , projectTitle :: Text
  , projectUrl :: Maybe Text
  , projectIssueTrackerIds :: Maybe [Text]
  , projectPolicyId :: Maybe Int
  , projectLatestRevision :: Maybe Text
  , projectDefaultBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON Project where
  parseJSON = withObject "ProjectResponse" $ \obj ->
    Project
      <$> obj .: "locator"
      <*> obj .: "title"
      <*> obj .: "url"
      <*> obj .: "issueTrackerProjectIds"
      <*> obj .: "policyId"
      <*> obj .: "last_analyzed_revision"
      <*> obj .: "default_branch"

data UpdateProjectRequest = UpdateProjectRequest
  { updateProjectTitle :: Maybe Text
  , updateProjectUrl :: Maybe Text
  , updateProjectIssueTrackerIds :: Maybe [Text]
  , updateProjectLabelIds :: Maybe [Int]
  , updateProjectPolicyId :: Maybe Int
  , -- This field needs to be set, otherwise the default branch will be removed
    updateProjectDefaultBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance ToJSON UpdateProjectRequest where
  toJSON UpdateProjectRequest{..} =
    object
      [ "title" .= maybe Null toJSON updateProjectTitle
      , "url" .= maybe Null toJSON updateProjectUrl
      , "issueTrackerProjectIds" .= maybe Null toJSON updateProjectIssueTrackerIds
      , "labels" .= maybe Null toJSON updateProjectLabelIds
      , "policyId" .= maybe Null toJSON updateProjectPolicyId
      , "default_branch" .= maybe Null toJSON updateProjectDefaultBranch
      ]

-- Revision
data Revision = Revision
  { revisionLocator :: Text
  , revisionLink :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON Revision where
  parseJSON = withObject "Revision" $ \obj ->
    Revision
      <$> obj .: "locator"
      <*> obj .: "link"

newtype UpdateRevisionRequest = UpdateRevisionRequest {newRevisionLink :: Text}
  deriving (Eq, Ord, Show)

instance ToJSON UpdateRevisionRequest where
  toJSON UpdateRevisionRequest{..} =
    object
      ["link" .= newRevisionLink]

-- Label
data Label = Label
  { labelId :: Int
  , labelName :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON Label where
  parseJSON = withObject "Label " $ \obj ->
    Label
      <$> obj .: "id"
      <*> obj .: "label"

newtype Labels = Labels {labels :: [Label]}
  deriving (Eq, Ord, Show)

instance FromJSON Labels where
  parseJSON = withObject "Labels" $ \obj ->
    Labels
      <$> obj .: "labels"
