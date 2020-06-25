module App.Fossa.CliTypes
  ( ApiKey (..),
    OverrideProject (..),
    ProjectRevision (..)
  )
where

import Prologue

newtype ApiKey = ApiKey {unApiKey :: Text} deriving (Eq, Ord, Show)

data OverrideProject = OverrideProject
  { overrideName :: Maybe Text,
    overrideRevision :: Maybe Text,
    overrideBranch :: Maybe Text
  }

data ProjectRevision = ProjectRevision
  { projectName :: Text
  , projectRevision :: Text
  , projectBranch :: Text
  } deriving (Eq, Ord, Show)