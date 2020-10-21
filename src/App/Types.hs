module App.Types
  ( ApiKey (..),
    BaseDir (..),
    NinjaGraphCLIOptions (..),
    OverrideProject (..),
    ProjectRevision (..),
  )
where

import Data.Text (Text)
import Path

newtype ApiKey = ApiKey {unApiKey :: Text} deriving (Eq, Ord, Show)
newtype BaseDir = BaseDir {unBaseDir :: Path Abs Dir} deriving (Eq, Ord, Show)

data OverrideProject = OverrideProject
  { overrideName :: Maybe Text,
    overrideRevision :: Maybe Text,
    overrideBranch :: Maybe Text
  }

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