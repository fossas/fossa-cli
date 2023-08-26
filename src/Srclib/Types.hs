{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Srclib.Types (
  SourceUnit (..),
  SourceUnitBuild (..),
  SourceUnitDependency (..),
  AdditionalDepData (..),
  SourceUserDefDep (..),
  SourceRemoteDep (..),
  Locator (..),
  LicenseSourceUnit (..),
  LicenseScanType (..),
  LicenseUnit (..),
  LicenseUnitData (..),
  LicenseUnitInfo (..),
  LicenseUnitMatchData (..),
  FullSourceUnit (..),
  OriginPath,
  renderLocator,
  parseLocator,
  pathToOriginPath,
  someBaseToOriginPath,
  somePathToOriginPath,
  emptyLicenseUnit,
  emptyLicenseUnitData,
  sourceUnitToFullSourceUnit,
  licenseUnitToFullSourceUnit,
) where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.String.Conversion (ToText, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Path (File, Path, SomeBase (..), toFilePath)
import Path.Extra (SomePath (..))
import Types (GraphBreadth (..))

data LicenseScanType = CliLicenseScanned
  deriving (Eq, Ord, Show)

instance ToText LicenseScanType where
  toText CliLicenseScanned = "cli-license-scanned"

instance ToJSON LicenseScanType where
  toJSON = toJSON . toText

-- |This type is meant to represent the paths in a project where a particular set of dependencies were discovered.
-- For example, in a project directory that has a @go.mod@ the OriginPath might be 'foo/bar/go.mod'.
-- In a project with VSI dependencies the OriginPath would be the directory the dep was found in, such as 'vendored/zlib/'
--
-- OriginPaths were previously `SomeBase File`, however with support for VSI OriginPaths can now be a directory in addition to a file path.
-- The reason we cannot use `SomePath` for this is that outputting an `OriginPath` to JSON in a form like @/foo/bar/path_end@ doesn't say whether the path is a file or directory which is required for parsing to a `SomePath` in `FromJSON`.
-- This type and its exported smart constructors describe that OriginPath is a path, but has no information about whether the path is a file or directory.
newtype OriginPath = OriginPath FilePath
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, ToText)

pathToOriginPath :: Path b t -> OriginPath
pathToOriginPath = OriginPath . toFilePath

someBaseToOriginPath :: SomeBase b -> OriginPath
someBaseToOriginPath (Abs p) = pathToOriginPath p
someBaseToOriginPath (Rel p) = pathToOriginPath p

somePathToOriginPath :: SomePath -> OriginPath
somePathToOriginPath (SomeFile f) = someBaseToOriginPath f
somePathToOriginPath (SomeDir d) = someBaseToOriginPath d

-- export interface SourceUnit {
--   Name?: string;
--   Type?: string;
--   Repo?: string;
--   Dir?: string;
--   Ops?: {
--     scan?: any;
--     depresolve?: any;
--     graph?: any;
--   };
--   ThemisVersion?: string;
--   SourceUnits?: SourceUnit[];
--   LicenseUnits?: SourceUnit[];
--   Info?: any;
--   Data?: SourceUnitNomosData[] | any;
--   Licenses?: { [licenseId: string]: SourceUnitLicenseMatch[] }
--   DeclaredLicenses?: SourceUnitLicense[];
--   HiddenLicenses?: HiddenLicense[];
--   NoticeFiles?: { [noticeFilePath: string]: string };
--   Dependencies?: SourceUnitDependency[];
--   DependencyLocks?: DependencyLock[];
--   Build?: SourceUnitBuild;
--   OriginPaths?: string[];
--   AdditionalDependencyData?: {
--     UserDefinedDependencies?: CLIUserDefinedDependency[];
--     RemoteDependencies?: RemoteDependency[];
--   };

--   /**
--    * Represents Manifest attribute
--    *
--    * For older CLI version submitted source unit,
--    * It provides equivalent value to @OriginPaths
--    */
--   Manifest?: string;
-- }

data FullSourceUnit = FullSourceUnit
  { fullSourceUnitName :: Text
  , fullSourceUnitType :: Text
  , fullSourceUnitTitle :: Maybe Text
  , fullSourceUnitManifest :: Maybe Text
  , fullSourceUnitBuild :: Maybe SourceUnitBuild
  , fullSourceUnitGraphBreadth :: GraphBreadth
  , fullSourceUnitOriginPaths :: [OriginPath]
  , fullSourceUnitAdditionalData :: Maybe AdditionalDepData
  , fullSourceUnitFiles :: Maybe (NonEmpty Text)
  , fullSourceUnitData :: Maybe (NonEmpty LicenseUnitData)
  , fullSourceUnitInfo :: Maybe LicenseUnitInfo
  }
  deriving (Eq, Ord, Show)

licenseUnitToFullSourceUnit :: LicenseUnit -> FullSourceUnit
licenseUnitToFullSourceUnit LicenseUnit{..} =
  FullSourceUnit
    { fullSourceUnitName = licenseUnitName
    , fullSourceUnitType = licenseUnitType
    , fullSourceUnitTitle = licenseUnitTitle
    , fullSourceUnitManifest = Nothing
    , fullSourceUnitBuild = Nothing
    , fullSourceUnitGraphBreadth = Complete
    , fullSourceUnitOriginPaths = []
    , fullSourceUnitAdditionalData = Nothing
    , fullSourceUnitFiles = Just licenseUnitFiles
    , fullSourceUnitData = Just licenseUnitData
    , fullSourceUnitInfo = Just licenseUnitInfo
    }

sourceUnitToFullSourceUnit :: SourceUnit -> FullSourceUnit
sourceUnitToFullSourceUnit SourceUnit{..} =
  FullSourceUnit
    { fullSourceUnitName = sourceUnitName
    , fullSourceUnitType = sourceUnitType
    , fullSourceUnitTitle = Nothing
    , fullSourceUnitManifest = Just sourceUnitManifest
    , fullSourceUnitBuild = sourceUnitBuild
    , fullSourceUnitGraphBreadth = sourceUnitGraphBreadth
    , fullSourceUnitOriginPaths = sourceUnitOriginPaths
    , fullSourceUnitAdditionalData = additionalData
    , fullSourceUnitFiles = Nothing
    , fullSourceUnitData = Nothing
    , fullSourceUnitInfo = Nothing
    }

instance ToJSON FullSourceUnit where
  toJSON FullSourceUnit{..} =
    object
      [ "Name" .= fullSourceUnitName
      , "Type" .= fullSourceUnitType
      , "Title" .= fullSourceUnitTitle
      , "Manifest" .= fullSourceUnitManifest
      , "Build" .= fullSourceUnitBuild
      , "GraphBreadth" .= fullSourceUnitGraphBreadth
      , "OriginPaths" .= fullSourceUnitOriginPaths
      , "AdditionalDependencyData" .= fullSourceUnitAdditionalData
      , "Files" .= fullSourceUnitFiles
      , "Data" .= fullSourceUnitData
      , "Info" .= fullSourceUnitInfo
      ]

-- | LicenseSourceUnit is the base of the results sent to Core for a CLI-side license scan
-- licenseSourceUnitLicenseUnits will be empty if you scan an empty directory.
data LicenseSourceUnit = LicenseSourceUnit
  { licenseSourceUnitName :: Text
  , licenseSourceUnitType :: LicenseScanType
  , licenseSourceUnitLicenseUnits :: NonEmpty LicenseUnit
  }
  deriving (Eq, Ord, Show)

instance Semigroup LicenseSourceUnit where
  LicenseSourceUnit name unitType licenseUnits1 <> LicenseSourceUnit _ _ licenseUnits2 = LicenseSourceUnit name unitType $ licenseUnits1 <> licenseUnits2

instance ToJSON LicenseSourceUnit where
  toJSON LicenseSourceUnit{..} =
    object
      [ "Name" .= licenseSourceUnitName
      , "Type" .= licenseSourceUnitType
      , "LicenseUnits" .= licenseSourceUnitLicenseUnits
      ]

-- There will be one of these for each license type found by Themis
-- The data returned from themis-cli is an array of these.
data LicenseUnit = LicenseUnit
  { licenseUnitName :: Text
  , licenseUnitType :: Text
  , licenseUnitTitle :: Maybe Text
  , licenseUnitDir :: Text
  , licenseUnitFiles :: (NonEmpty Text)
  , licenseUnitData :: (NonEmpty LicenseUnitData)
  , licenseUnitInfo :: LicenseUnitInfo
  }
  deriving (Eq, Ord, Show)

emptyLicenseUnit :: LicenseUnit
emptyLicenseUnit =
  LicenseUnit
    { licenseUnitName = "empty"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitTitle = Nothing
    , licenseUnitDir = ""
    , licenseUnitFiles = "" :| []
    , licenseUnitData = emptyLicenseUnitData :| []
    , licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Nothing}
    }

instance ToJSON LicenseUnit where
  toJSON LicenseUnit{..} =
    object
      [ "Name" .= licenseUnitName
      , "Type" .= licenseUnitType
      , "Dir" .= licenseUnitDir
      , "Files" .= licenseUnitFiles
      , "Data" .= licenseUnitData
      , "Info" .= licenseUnitInfo
      ]

instance FromJSON LicenseUnit where
  parseJSON = withObject "LicenseUnit" $ \obj ->
    LicenseUnit
      <$> obj .: "Name"
      <*> obj .: "Type"
      <*> obj .:? "Title"
      <*> obj .: "Dir"
      <*> obj .: "Files"
      <*> obj .: "Data"
      <*> obj .: "Info"

newtype LicenseUnitInfo = LicenseUnitInfo
  {licenseUnitInfoDescription :: Maybe Text}
  deriving (Eq, Ord, Show)

instance ToJSON LicenseUnitInfo where
  toJSON LicenseUnitInfo{..} =
    object ["Description" .= licenseUnitInfoDescription]

instance FromJSON LicenseUnitInfo where
  parseJSON = withObject "LicenseUnitInfo" $ \obj ->
    LicenseUnitInfo <$> obj .: "Description"

-- | LicenseUnitData contains data about a license unit. At least one of licenseUnitDataMatchData or licenseUnitDataCopyrights will be non-empty
data LicenseUnitData = LicenseUnitData
  { licenseUnitDataPath :: Text
  , licenseUnitDataCopyright :: Maybe Text
  , licenseUnitDataThemisVersion :: Text
  , licenseUnitDataMatchData :: Maybe (NonEmpty LicenseUnitMatchData)
  , licenseUnitDataCopyrights :: Maybe (NonEmpty Text)
  , licenseUnitDataContents :: Maybe Text
  }
  deriving (Eq, Ord, Show)

emptyLicenseUnitData :: LicenseUnitData
emptyLicenseUnitData =
  LicenseUnitData
    { licenseUnitDataPath = ""
    , licenseUnitDataCopyright = Nothing
    , licenseUnitDataThemisVersion = ""
    , licenseUnitDataMatchData = Nothing
    , licenseUnitDataCopyrights = Nothing
    , licenseUnitDataContents = Nothing
    }

instance ToJSON LicenseUnitData where
  toJSON LicenseUnitData{..} =
    object
      [ "path" .= licenseUnitDataPath
      , "Copyright" .= licenseUnitDataCopyright
      , "ThemisVersion" .= licenseUnitDataThemisVersion
      , "match_data" .= licenseUnitDataMatchData
      , "Copyrights" .= licenseUnitDataCopyrights
      , "Contents" .= licenseUnitDataContents
      ]

instance FromJSON LicenseUnitData where
  parseJSON = withObject "LicenseUnitData" $ \obj ->
    LicenseUnitData
      <$> obj .: "path"
      <*> obj .:? "Copyright"
      <*> obj .: "ThemisVersion"
      <*> obj .:? "match_data"
      <*> obj .:? "Copyrights"
      <*> obj .:? "Contents"

data LicenseUnitMatchData = LicenseUnitMatchData
  { licenseUnitMatchDataMatchString :: Maybe Text
  , licenseUnitMatchDataLocation :: Integer
  , licenseUnitMatchDataLength :: Integer
  , licenseUnitMatchDataIndex :: Integer
  , licenseUnitDataStartLine :: Integer
  , licenseUnitDataEndLine :: Integer
  }
  deriving (Eq, Ord, Show)

instance ToJSON LicenseUnitMatchData where
  toJSON LicenseUnitMatchData{..} =
    object
      [ "match_string" .= licenseUnitMatchDataMatchString
      , "location" .= licenseUnitMatchDataLocation
      , "length" .= licenseUnitMatchDataLength
      , "index" .= licenseUnitMatchDataIndex
      , "start_line" .= licenseUnitDataStartLine
      , "end_line" .= licenseUnitDataEndLine
      ]

instance FromJSON LicenseUnitMatchData where
  parseJSON = withObject "LicenseUnitMatchData" $ \obj ->
    LicenseUnitMatchData
      <$> obj .:? "match_string"
      <*> obj .: "location"
      <*> obj .: "length"
      <*> obj .: "index"
      <*> obj .: "start_line"
      <*> obj .: "end_line"

data SourceUnit = SourceUnit
  { sourceUnitName :: Text
  , sourceUnitType :: Text
  , -- | path to manifest file
    sourceUnitManifest :: Text
  , sourceUnitBuild :: Maybe SourceUnitBuild
  , sourceUnitGraphBreadth :: GraphBreadth
  , sourceUnitOriginPaths :: [OriginPath]
  , additionalData :: Maybe AdditionalDepData
  }
  deriving (Eq, Ord, Show)

data SourceUnitBuild = SourceUnitBuild
  { -- | always "default"
    buildArtifact :: Text
  , -- | always true
    buildSucceeded :: Bool
  , buildImports :: [Locator]
  , buildDependencies :: [SourceUnitDependency]
  }
  deriving (Eq, Ord, Show)

data SourceUnitDependency = SourceUnitDependency
  { sourceDepLocator :: Locator
  , sourceDepImports :: [Locator] -- omitempty
  -- , sourceDepData :: Aeson.Value
  }
  deriving (Eq, Ord, Show)

data AdditionalDepData = AdditionalDepData
  { userDefinedDeps :: Maybe [SourceUserDefDep]
  , remoteDeps :: Maybe [SourceRemoteDep]
  }
  deriving (Eq, Ord, Show)

data SourceUserDefDep = SourceUserDefDep
  { srcUserDepName :: Text
  , srcUserDepVersion :: Text
  , srcUserDepLicense :: Text
  , srcUserDepDescription :: Maybe Text
  , srcUserDepHomepage :: Maybe Text
  , srcUserDepOrigin :: Maybe (SomeBase File)
  }
  deriving (Eq, Ord, Show)

data SourceRemoteDep = SourceRemoteDep
  { srcRemoteDepName :: Text
  , srcRemoteDepVersion :: Text
  , srcRemoteDepUrl :: Text
  , srcRemoteDepDescription :: Maybe Text
  , srcRemoteDepHomepage :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data Locator = Locator
  { locatorFetcher :: Text
  , locatorProject :: Text
  , locatorRevision :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance ToText Locator where
  toText = renderLocator

renderLocator :: Locator -> Text
renderLocator Locator{..} =
  locatorFetcher <> "+" <> locatorProject <> "$" <> fromMaybe "" locatorRevision

parseLocator :: Text -> Locator
parseLocator raw = Locator fetcher project (if Text.null revision then Nothing else Just revision)
  where
    (fetcher, xs) = Text.breakOn "+" raw
    (project, xs') = Text.breakOn "$" (Text.drop 1 xs)
    revision = Text.drop 1 xs'

instance ToJSON SourceUnit where
  toJSON SourceUnit{..} =
    object
      [ "Name" .= sourceUnitName
      , "Type" .= sourceUnitType
      , "Manifest" .= sourceUnitManifest
      , "Build" .= sourceUnitBuild
      , "GraphBreadth" .= sourceUnitGraphBreadth
      , "OriginPaths" .= sourceUnitOriginPaths
      , "AdditionalDependencyData" .= additionalData
      ]

instance FromJSON SourceUnit where
  parseJSON = withObject "SourceUnit" $ \obj ->
    SourceUnit
      <$> obj .: "Name"
      <*> obj .: "Type"
      <*> obj .: "Manifest"
      <*> obj .:? "Build"
      <*> obj .: "GraphBreadth"
      <*> obj .: "OriginPaths"
      <*> obj .:? "AdditionalDependencyData"

instance ToJSON SourceUnitBuild where
  toJSON SourceUnitBuild{..} =
    object
      [ "Artifact" .= buildArtifact
      , "Succeeded" .= buildSucceeded
      , "Imports" .= buildImports
      , "Dependencies" .= buildDependencies
      ]

instance FromJSON SourceUnitBuild where
  parseJSON = withObject "SourceUnitBuild" $ \obj ->
    SourceUnitBuild
      <$> obj .: "Artifact"
      <*> obj .: "Succeeded"
      <*> obj .: "Imports"
      <*> obj .: "Dependencies"

instance ToJSON SourceUnitDependency where
  toJSON SourceUnitDependency{..} =
    object
      [ "locator" .= sourceDepLocator
      , "imports" .= sourceDepImports
      ]

instance FromJSON SourceUnitDependency where
  parseJSON = withObject "SourceUnitDependency" $ \obj ->
    SourceUnitDependency
      <$> obj .: "locator"
      <*> obj .: "imports"

instance ToJSON AdditionalDepData where
  toJSON AdditionalDepData{..} =
    object
      [ "UserDefinedDependencies" .= userDefinedDeps
      , "RemoteDependencies" .= remoteDeps
      ]

instance FromJSON AdditionalDepData where
  parseJSON = withObject "AdditionalDepData" $ \obj ->
    AdditionalDepData
      <$> obj .:? "UserDefinedDependencies"
      <*> obj .:? "RemoteDependencies"

instance ToJSON SourceUserDefDep where
  toJSON SourceUserDefDep{..} =
    object
      [ "Name" .= srcUserDepName
      , "Version" .= srcUserDepVersion
      , "License" .= srcUserDepLicense
      , "Description" .= srcUserDepDescription
      , "Homepage" .= srcUserDepHomepage
      , "Origin" .= fmap toText srcUserDepOrigin
      ]

instance FromJSON SourceUserDefDep where
  parseJSON = withObject "SourceUserDefDep" $ \obj ->
    SourceUserDefDep
      <$> obj .: "Name"
      <*> obj .: "Version"
      <*> obj .: "License"
      <*> obj .:? "Description"
      <*> obj .:? "Homepage"
      <*> obj .:? "Origin"

instance ToJSON SourceRemoteDep where
  toJSON SourceRemoteDep{..} =
    object
      [ "Name" .= srcRemoteDepName
      , "Version" .= srcRemoteDepVersion
      , "Url" .= srcRemoteDepUrl
      , "Description" .= srcRemoteDepDescription
      , "Homepage" .= srcRemoteDepHomepage
      ]

instance FromJSON SourceRemoteDep where
  parseJSON = withObject "SourceRemoteDep" $ \obj ->
    SourceRemoteDep
      <$> obj .: "Name"
      <*> obj .: "Version"
      <*> obj .: "Url"
      <*> obj .:? "Description"
      <*> obj .:? "Homepage"

instance ToJSON Locator where
  -- render as text
  toJSON = toJSON . renderLocator

instance FromJSON Locator where
  parseJSON = withText "Locator" (pure . parseLocator)
