{-# LANGUAGE RecordWildCards #-}

module App.Types (
  BaseDir (..),
  NinjaGraphCLIOptions (..),
  OverrideProject (..),
  ProjectMetadata (..),
  ReleaseGroupMetadata (..),
  ProjectRevision (..),
  LocatorType (..),
  OverrideDynamicAnalysisBinary (..),
  Policy (..),
  DependencyRebuild (..),
  FileUpload (..),
  FirstPartyScansFlag (..),
  ReleaseGroupRevision (..),
  ReleaseGroupProjectRevision (..),
  ReleaseGroupReleaseRevision (..),
  ComponentUploadFileType (..),
  uploadFileTypeToFetcherName,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), defaultOptions, genericToEncoding, object, withObject, (.:), (.=))
import Data.Aeson.Types (toJSON)
import Data.Map (Map)
import Data.String.Conversion (ToText (..), showText)
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

data Policy
  = PolicyName Text
  | PolicyId Int
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Policy where
  toEncoding = genericToEncoding defaultOptions

data ProjectMetadata = ProjectMetadata
  { projectTitle :: Maybe Text
  , projectUrl :: Maybe Text
  , projectJiraKey :: Maybe Text
  , projectLink :: Maybe Text
  , projectTeam :: Maybe Text
  , projectPolicy :: Maybe Policy
  , projectLabel :: [Text]
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
      <$> obj .: "name"
      <*> obj .: "release"

data ProjectRevision = ProjectRevision
  { projectName :: Text
  , projectRevision :: Text
  , projectBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

data LocatorType = LocatorTypeCustom | LocatorTypeSBOM
  deriving (Eq, Ord, Show, Generic)

instance ToText LocatorType where
  toText LocatorTypeCustom = "custom"
  toText LocatorTypeSBOM = "sbom"

instance ToJSON LocatorType where
  toEncoding LocatorTypeCustom = "custom"
  toEncoding LocatorTypeSBOM = "sbom"

instance ToJSON ProjectRevision where
  toEncoding = genericToEncoding defaultOptions

data ComponentUploadFileType = ArchiveUpload | SBOMUpload
  deriving (Eq, Ord, Show, Generic)

uploadFileTypeToFetcherName :: ComponentUploadFileType -> Text
uploadFileTypeToFetcherName ArchiveUpload = "archive"
uploadFileTypeToFetcherName SBOMUpload = "sbom"

data ReleaseGroupRevision = ReleaseGroupRevision
  { releaseGroupTitle :: Text
  , releaseGroupReleaseRevision :: ReleaseGroupReleaseRevision
  , releaseGroupLicensePolicy :: Maybe Text
  , releaseGroupSecurityPolicy :: Maybe Text
  , releaseGroupQualityPolicy :: Maybe Text
  , releaseGroupTeams :: Maybe [Text]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReleaseGroupRevision where
  toJSON ReleaseGroupRevision{..} =
    object
      [ "title" .= releaseGroupTitle
      , "release" .= releaseGroupReleaseRevision
      , "licensePolicy" .= releaseGroupLicensePolicy
      , "securityPolicy" .= releaseGroupSecurityPolicy
      , "teams" .= releaseGroupTeams
      ]

data ReleaseGroupReleaseRevision = ReleaseGroupReleaseRevision
  { releaseTitle :: Text
  , releaseProjects :: [ReleaseGroupProjectRevision]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReleaseGroupReleaseRevision where
  toJSON ReleaseGroupReleaseRevision{..} =
    object
      [ "title" .= releaseTitle
      , "projects" .= releaseProjects
      ]

data ReleaseGroupProjectRevision = ReleaseGroupProjectRevision
  { releaseGroupProjectLocator :: Text
  , releaseGroupProjectRevision :: Text
  , releaseGroupProjectBranch :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReleaseGroupProjectRevision where
  toJSON ReleaseGroupProjectRevision{..} =
    object
      [ "projectId" .= releaseGroupProjectLocator
      , "revisionId" .= releaseGroupProjectRevision
      , "branch" .= releaseGroupProjectBranch
      ]

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

data DependencyRebuild
  = DependencyRebuildReuseCache
  | DependencyRebuildInvalidateCache
  deriving (Eq, Ord, Show, Generic)

instance ToText DependencyRebuild where
  toText = showText

instance ToJSON DependencyRebuild where
  toJSON DependencyRebuildReuseCache = "reuse_cache"
  toJSON DependencyRebuildInvalidateCache = "invalidate_cache"

data FileUpload
  = FileUploadMatchData
  | FileUploadFullContent
  deriving (Eq, Ord, Show, Generic)

instance ToText FileUpload where
  toText FileUploadMatchData = "match_data"
  toText FileUploadFullContent = "full_files"

instance ToJSON FileUpload where
  toJSON FileUploadMatchData = "match_data"
  toJSON FileUploadFullContent = "full_files"

data FirstPartyScansFlag = FirstPartyScansOnFromFlag | FirstPartyScansOffFromFlag | FirstPartyScansUseDefault
  deriving (Eq, Ord, Show, Generic)

instance ToJSON FirstPartyScansFlag where
  toEncoding = genericToEncoding defaultOptions
