module App.Types
  ( ApiKey (..),
    BaseDir (..),
    NinjaGraphCLIOptions (..),
    OverrideProject (..),
    ProjectMetadata (..),
    ProjectRevision (..),
    UploadInfo (..),
  )
where

import Data.Text (Text)
import Text.URI
import Path

newtype ApiKey = ApiKey {unApiKey :: Text} deriving (Eq, Ord, Show)
newtype BaseDir = BaseDir {unBaseDir :: Path Abs Dir} deriving (Eq, Ord, Show)

data UploadInfo = UploadInfo
  { uploadUri :: URI,
    uploadApiKey :: ApiKey,
    uploadMetadata :: ProjectMetadata
  }

data OverrideProject = OverrideProject
  { overrideName :: Maybe Text,
    overrideRevision :: Maybe Text,
    overrideBranch :: Maybe Text
  }

data ProjectMetadata = ProjectMetadata
  { projectTitle :: Maybe Text
  , projectUrl :: Maybe Text
  , projectJiraKey :: Maybe Text
  , projectLink :: Maybe Text
  , projectTeam :: Maybe Text
  , projectPolicy :: Maybe Text
  } deriving (Eq, Ord, Show)

data ProjectRevision = ProjectRevision
  { projectName :: Text
  , projectRevision :: Text
  , projectBranch :: Maybe Text
  } deriving (Eq, Ord, Show)

data NinjaGraphCLIOptions = NinjaGraphCLIOptions
  { ninjaBaseDir :: FilePath,
    ninjaDepsFile :: Maybe FilePath,
    ninjaLunchTarget :: Maybe Text,
    ninjaScanId :: Text,
    ninjaBuildName :: Text
  }