module Strategy.Python.PyProjectGeneric.Types
  ( PyProjectType (..)
  , LockFileType (..)
  , PyProjectGeneric (..)
  , PyProjectMetadata (..)
  , PyProjectBuildSystem (..)
  , PyProjectPoetry (..)
  , PyProjectPDM (..)
  , PoetryDependency (..)
  , PyProjectDetailedVersionDependency (..)
  , PyProjectGitDependency (..)
  , PyProjectPathDependency (..)
  , PyProjectUrlDependency (..)
  , detectProjectType
  , projectTypePriority
  , prioritizeProjectType
  , isPEP621
  , isPoetry
  , isPDM
  , dependencyVersion
  , gitUrl
  , sourcePath
  , sourceUrl
  ) where

import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.Aeson.Types qualified as Aeson
import Data.List (sortOn)
import Data.Map (Map)
import Data.Text (Text)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Strategy.Python.Util (Req)
import Toml qualified
import Toml.Schema qualified

-- | Enumeration of PyProject types
data PyProjectType
  = PoetryProject
  | PDMProject
  | PEP621Project
  | UnknownProject
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectType where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

-- | Types of lock files
data LockFileType 
  = PoetryLock 
  | PDMLock 
  | UVLock     -- For future support
  | OtherLock Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LockFileType where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

-- | Define priority order for project types (highest to lowest)
projectTypePriority :: PyProjectType -> Int
projectTypePriority = \case
  PoetryProject -> 3    -- Highest priority
  PDMProject -> 2
  PEP621Project -> 1
  UnknownProject -> 0   -- Lowest priority

-- | Priority-based project type selection
-- When multiple project types are detected, choose the highest priority one
-- Note: While a pyproject.toml file can contain configuration for multiple build systems,
-- in practice, developers typically choose one primary build system per project. This
-- prioritization logic ensures we select the most likely active build system when
-- multiple configurations are present, avoiding confusion and duplicate analysis results.
-- Currently, we don't add special handling for truly mixed build systems.
prioritizeProjectType :: [PyProjectType] -> PyProjectType
prioritizeProjectType candidates =
  case sortOn (negate . projectTypePriority) candidates of
    (highestPriority:_) -> highestPriority
    [] -> UnknownProject

-- | Tool-specific sections representation
data PyProjectTool = PyProjectTool
  { poetryTool :: Maybe PyProjectPoetry
  , pdmTool :: Maybe PyProjectPDM
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectTool where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Toml.Schema.FromValue PyProjectTool where
  fromValue = 
    Toml.Schema.parseTableFromValue $
      PyProjectTool
        <$> Toml.Schema.optKey "poetry"
        <*> Toml.Schema.optKey "pdm"

-- | Generic PyProject.toml representation
data PyProjectGeneric = PyProjectGeneric
  { -- Common PEP 621 fields
    projectMetadata :: Maybe PyProjectMetadata
    -- Tool-specific sections
  , poetrySection :: Maybe PyProjectPoetry
  , pdmSection :: Maybe PyProjectPDM
  , buildSystem :: Maybe PyProjectBuildSystem
  , projectType :: PyProjectType
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectGeneric where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Toml.Schema.FromValue PyProjectGeneric where
  fromValue v = do
    -- Parse the pyproject.toml
    result <- Toml.Schema.parseTableFromValue
      (do
        pMetadata <- Toml.Schema.optKey "project"
        toolSection <- Toml.Schema.optKey "tool"
        bSystem <- Toml.Schema.optKey "build-system"
        
        -- Extract tool-specific sections if tool section exists
        let poetryInfo = case toolSection of
                          Just tool -> poetryTool tool
                          Nothing -> Nothing
        let pdmInfo = case toolSection of
                        Just tool -> pdmTool tool
                        Nothing -> Nothing
                
        -- Create the PyProjectGeneric
        pure $ PyProjectGeneric 
          { projectMetadata = pMetadata
          , poetrySection = poetryInfo 
          , pdmSection = pdmInfo
          , buildSystem = bSystem
          , projectType = UnknownProject
          }
      ) v

    -- Set the project type and return
    pure $ result { projectType = detectProjectType result }

-- | Represents PEP 621 project metadata
data PyProjectMetadata = PyProjectMetadata
  { projectName :: Maybe Text
  , projectVersion :: Maybe Text
  , projectDescription :: Maybe Text
  , projectDependencies :: Maybe [Req]
  , projectOptionalDependencies :: Maybe (Map Text [Req])
  }
  deriving (Eq, Ord, Show, Generic)

-- Define a simple ToJSON instance that only includes fields that can be simply serialized
instance ToJSON PyProjectMetadata where
  toJSON metadata = object
    [ "name" .= projectName metadata
    , "version" .= projectVersion metadata
    , "description" .= projectDescription metadata
    ]

instance Toml.Schema.FromValue PyProjectMetadata where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectMetadata
        <$> Toml.Schema.optKey "name"
        <*> Toml.Schema.optKey "version"
        <*> Toml.Schema.optKey "description"
        <*> Toml.Schema.optKey "dependencies"
        <*> Toml.Schema.optKey "optional-dependencies"

-- | Poetry-specific configuration
data PyProjectPoetry = PyProjectPoetry
  { poetryName :: Maybe Text
  , poetryVersion :: Maybe Text
  , poetryDescription :: Maybe Text
  , poetryDependencies :: Map Text PoetryDependency
  , poetryDevDependencies :: Map Text PoetryDependency
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectPoetry where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Toml.Schema.FromValue PyProjectPoetry where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPoetry
        <$> Toml.Schema.optKey "name"
        <*> Toml.Schema.optKey "version"
        <*> Toml.Schema.optKey "description"
        <*> Toml.Schema.pickKey [Toml.Schema.Key "dependencies" Toml.Schema.fromValue, Toml.Schema.Else (pure mempty)]
        <*> Toml.Schema.pickKey [Toml.Schema.Key "dev-dependencies" Toml.Schema.fromValue, Toml.Schema.Else (pure mempty)]

-- | Poetry dependency types
data PoetryDependency
  = PoetryTextVersion Text
  | PoetryDetailedVersion PyProjectDetailedVersionDependency
  | PoetryGitDependency PyProjectGitDependency
  | PoetryPathDependency PyProjectPathDependency
  | PoetryUrlDependency PyProjectUrlDependency
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PoetryDependency where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Toml.Schema.FromValue PoetryDependency where
  fromValue (Toml.Text' _ t) = pure $ PoetryTextVersion t
  fromValue v@(Toml.Table' l t) =
    Toml.Schema.parseTable
      ( Toml.Schema.pickKey
          [ Toml.Schema.Key "version" (const (PoetryDetailedVersion <$> Toml.Schema.fromValue v))
          , Toml.Schema.Key "git" (const (PoetryGitDependency <$> Toml.Schema.fromValue v))
          , Toml.Schema.Key "path" (const (PoetryPathDependency <$> Toml.Schema.fromValue v))
          , Toml.Schema.Key "url" (const (PoetryUrlDependency <$> Toml.Schema.fromValue v))
          , Toml.Schema.Else (Toml.Schema.failAt (Toml.valueAnn v) "invalid poetry dependency spec")
          ]
      )
      l
      t
  fromValue v = Toml.Schema.failAt (Toml.valueAnn v) $ "invalid poetry dependency" <> Toml.valueType v

-- | Detailed version dependency
newtype PyProjectDetailedVersionDependency = PyProjectDetailedVersionDependency
  { dependencyVersion :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectDetailedVersionDependency where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Toml.Schema.FromValue PyProjectDetailedVersionDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectDetailedVersionDependency
        <$> Toml.Schema.reqKey "version"

-- | Git dependency
data PyProjectGitDependency = PyProjectGitDependency
  { gitUrl :: Text
  , gitBranch :: Maybe Text
  , gitRev :: Maybe Text
  , gitTag :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectGitDependency where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Toml.Schema.FromValue PyProjectGitDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectGitDependency
        <$> Toml.Schema.reqKey "git"
        <*> Toml.Schema.optKey "branch"
        <*> Toml.Schema.optKey "rev"
        <*> Toml.Schema.optKey "tag"

-- | Path dependency
newtype PyProjectPathDependency = PyProjectPathDependency
  { sourcePath :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectPathDependency where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Toml.Schema.FromValue PyProjectPathDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPathDependency
        <$> Toml.Schema.reqKey "path"

-- | URL dependency
newtype PyProjectUrlDependency = PyProjectUrlDependency
  { sourceUrl :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectUrlDependency where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Toml.Schema.FromValue PyProjectUrlDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectUrlDependency
        <$> Toml.Schema.reqKey "url"

-- | PDM-specific configuration
data PyProjectPDM = PyProjectPDM
  { pdmDevDependencies :: Maybe (Map Text [Req])
  }
  deriving (Eq, Ord, Show, Generic)

-- Define simple ToJSON instance manually to avoid needing ToJSON Req
instance ToJSON PyProjectPDM where
  toJSON _ = object ["dev-dependencies" .= Aeson.Null]  -- Just an empty placeholder

instance Toml.Schema.FromValue PyProjectPDM where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectPDM
        <$> Toml.Schema.optKey "dev-dependencies"

-- | Build system configuration
newtype PyProjectBuildSystem = PyProjectBuildSystem
  { buildBackend :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectBuildSystem where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Toml.Schema.FromValue PyProjectBuildSystem where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PyProjectBuildSystem
        <$> Toml.Schema.optKey "build-backend"

-- | Detect project type from PyProject.toml
detectProjectType :: PyProjectGeneric -> PyProjectType
detectProjectType pyproject
  | isPoetry pyproject = PoetryProject
  | isPDM pyproject = PDMProject
  | isPEP621 pyproject = PEP621Project
  | otherwise = UnknownProject

-- | Check if project is Poetry
isPoetry :: PyProjectGeneric -> Bool
isPoetry = isJust . poetrySection

-- | Check if project is PDM 
isPDM :: PyProjectGeneric -> Bool
isPDM = isJust . pdmSection

-- | Check if project follows PEP 621
isPEP621 :: PyProjectGeneric -> Bool
isPEP621 = isJust . projectMetadata

-- | Debugging function to show project parsing
debugProject :: PyProjectGeneric -> String
debugProject proj = 
  "Poetry: " ++ show (isJust $ poetrySection proj) ++ 
  ", PDM: " ++ show (isJust $ pdmSection proj) ++ 
  ", PEP621: " ++ show (isJust $ projectMetadata proj)