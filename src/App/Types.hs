module App.Types (
  BaseDir (..),
  NinjaGraphCLIOptions (..),
  OverrideProject (..),
  ProjectMetadata (..),
  ProjectRevision (..),
) where

import Data.Text (Text)
import Path

newtype BaseDir = BaseDir {unBaseDir :: Path Abs Dir} deriving (Eq, Ord, Show)

data OverrideProject = OverrideProject
  { overrideName :: Maybe Text
  , overrideRevision :: Maybe Text
  , overrideBranch :: Maybe Text
  }

data ProjectMetadata = ProjectMetadata
  { projectTitle :: Maybe Text
  , projectUrl :: Maybe Text
  , projectJiraKey :: Maybe Text
  , projectLink :: Maybe Text
  , projectTeam :: Maybe Text
  , projectPolicy :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data ProjectRevision = ProjectRevision
  { projectName :: Text
  , projectRevision :: Text
  , projectBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data NinjaGraphCLIOptions = NinjaGraphCLIOptions
  { ninjaBaseDir :: FilePath
  , ninjaDepsFile :: Maybe FilePath
  , ninjaLunchTarget :: Maybe Text
  , ninjaScanId :: Text
  , ninjaBuildName :: Text
  }
