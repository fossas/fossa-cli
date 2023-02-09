module App.Types (
  BaseDir (..),
  NinjaGraphCLIOptions (..),
  OverrideProject (..),
  ProjectMetadata (..),
  ReleaseGroupMetadata (..),
  ProjectRevision (..),
  MonorepoAnalysisOpts (..),
  OverrideDynamicAnalysisBinary (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), defaultOptions, genericToEncoding, withObject, (.:))
import Data.Map (Map)
import Data.Text (Text)
import DepTypes (DepType)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)

newtype BaseDir = BaseDir {unBaseDir :: Path Abs Dir} deriving (Eq, Ord, Show, Generic)
instance ToJSON BaseDir where
  toEncoding = genericToEncoding defaultOptions

data OverrideProject = OverrideProject
  { overrideName :: Maybe Text
  , overrideRevision :: Maybe Text
  , overrideBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON OverrideProject where
  toEncoding = genericToEncoding defaultOptions

data ProjectMetadata = ProjectMetadata
  { projectTitle :: Maybe Text
  , projectUrl :: Maybe Text
  , projectJiraKey :: Maybe Text
  , projectLink :: Maybe Text
  , projectTeam :: Maybe Text
  , projectPolicy :: Maybe Text
  , projectReleaseGroup :: Maybe ReleaseGroupMetadata
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ProjectMetadata where
  toEncoding = genericToEncoding defaultOptions

data ReleaseGroupMetadata = ReleaseGroupMetadata
  { releaseGroupName :: Text
  , releaseGroupRelease :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReleaseGroupMetadata where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ReleaseGroupMetadata where
  parseJSON = withObject "ReleaseGroupMetadata" $ \obj ->
    ReleaseGroupMetadata
      <$> obj
        .: "name"
      <*> obj
        .: "release"

newtype MonorepoAnalysisOpts = MonorepoAnalysisOpts
  { monorepoAnalysisType :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MonorepoAnalysisOpts where
  toEncoding = genericToEncoding defaultOptions

data ProjectRevision = ProjectRevision
  { projectName :: Text
  , projectRevision :: Text
  , projectBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ProjectRevision where
  toEncoding = genericToEncoding defaultOptions

data NinjaGraphCLIOptions = NinjaGraphCLIOptions
  { ninjaBaseDir :: FilePath
  , ninjaDepsFile :: Maybe FilePath
  , ninjaLunchTarget :: Maybe Text
  , ninjaScanId :: Text
  , ninjaBuildName :: Text
  }

newtype OverrideDynamicAnalysisBinary = OverrideDynamicAnalysisBinary
  {unOverrideDynamicAnalysisBinary :: Map DepType Text}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON OverrideDynamicAnalysisBinary where
  toEncoding = genericToEncoding defaultOptions

instance Semigroup OverrideDynamicAnalysisBinary where
  (OverrideDynamicAnalysisBinary a1) <> (OverrideDynamicAnalysisBinary a2) = OverrideDynamicAnalysisBinary (a1 <> a2)

instance Monoid OverrideDynamicAnalysisBinary where
  mempty = OverrideDynamicAnalysisBinary mempty
