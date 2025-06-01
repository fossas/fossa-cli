{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Ficus.Types (
  OrgWideCustomLicenseConfigPolicy (..),
  GrepOptions (..),
  GrepEntry (..),
  FicusResults (..),
  FicusMatch (..),
  FicusWarning (..),
  FicusError (..),
  FicusMessage (..),
  FicusMessages (..),
  FicusConfig (..),
  FicusRegex (..),
  FicusMatchData (..),
  FicusScanType (..),
) where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), Value (Object), defaultOptions, genericToEncoding, object, withObject, withText, (.:?))
import Data.Aeson qualified as A
import Data.Aeson.Types ((.:))
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Srclib.Types (LicenseSourceUnit)
import Types (LicenseScanPathFilters (..))

data OrgWideCustomLicenseConfigPolicy = Use | Ignore
  deriving (Eq, Ord, Show)

instance Semigroup OrgWideCustomLicenseConfigPolicy where
  (<>) Use Use = Use
  (<>) _ _ = Ignore

instance ToText OrgWideCustomLicenseConfigPolicy where
  toText Use = "Use"
  toText Ignore = "Ignore"

instance ToJSON OrgWideCustomLicenseConfigPolicy where
  toJSON = toJSON . toText

data GrepOptions = GrepOptions
  { customLicenseSearch :: [GrepEntry]
  , keywordSearch :: [GrepEntry]
  , orgWideCustomLicenseScanConfigPolicy :: OrgWideCustomLicenseConfigPolicy
  , configFilePath :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GrepOptions where
  toEncoding = genericToEncoding defaultOptions

data GrepEntry = GrepEntry
  { grepEntryMatchCriteria :: Text
  , grepEntryName :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GrepEntry where
  toJSON (GrepEntry matchCriteria name) =
    object
      [ "matchCriteria" .= matchCriteria
      , "name" .= name
      ]

instance FromJSON GrepEntry where
  parseJSON = withObject "GrepEntry" $ \obj ->
    GrepEntry
      <$> obj .: "matchCriteria"
      <*> obj .: "name"

data FicusConfig = FicusConfig
  { rootDir :: Path Abs Dir
  , regexes :: [FicusRegex]
  , licenseScanPathFilters :: Maybe LicenseScanPathFilters
  , fullFiles :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON FicusConfig where
  toJSON FicusConfig{..} =
    object
      [ "root_dir" .= toText rootDir
      , "regexes" .= toJSON regexes
      , "license_scan_path_filters" .= toJSON licenseScanPathFilters
      , "full_files" .= fullFiles
      ]

data FicusRegex = FicusRegex
  { pat :: Text
  , name :: Text
  , scanType :: FicusScanType
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON FicusRegex where
  toJSON FicusRegex{..} =
    object
      [ "pattern" .= toText pat
      , "name" .= toText name
      , "scan_type" .= toText scanType
      ]

data FicusScanType = CustomLicense | KeywordSearch
  deriving (Eq, Ord, Show, Generic)

instance ToText FicusScanType where
  toText CustomLicense = "CustomLicense"
  toText KeywordSearch = "KeywordSearch"

instance ToJSON FicusScanType where
  toJSON = toJSON . toText

instance FromJSON FicusScanType

data LernieMessageType = LernieMessageTypeMatch | LernieMessageTypeError | LernieMessageTypeWarning
  deriving (Eq, Ord, Show, Generic)

instance ToText LernieMessageType where
  toText LernieMessageTypeMatch = "Match"
  toText LernieMessageTypeError = "Error"
  toText LernieMessageTypeWarning = "Warning"

instance ToJSON LernieMessageType where
  toJSON = toJSON . toText

instance FromJSON LernieMessageType where
  parseJSON = withText "LernieMessageType" $ \msg -> do
    case msg of
      "Match" -> pure LernieMessageTypeMatch
      "Error" -> pure LernieMessageTypeError
      "Warning" -> pure LernieMessageTypeWarning
      _ -> fail "invalid Lernie message type"

data FicusMatch = FicusMatch
  { ficusMatchPath :: Text
  , ficusMatchMatches :: [FicusMatchData]
  , ficusMatchContents :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieMatch where
  parseJSON = withObject "LernieMatch" $ \obj ->
    LernieMatch
      <$> (obj .: "path")
      <*> (obj .: "matches")
      <*> (obj .:? "contents")

data FicusWarning = FicusWarning
  { ficusWarningMessage :: Text
  , ficusWarningType :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieWarning where
  parseJSON = withObject "LernieWarning" $ \obj ->
    LernieWarning
      <$> (obj .: "message")
      <*> (obj .: "type")

data FicusError = FicusError
  { ficusErrorMessage :: Text
  , ficusErrorType :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieError where
  parseJSON = withObject "LernieError" $ \obj ->
    LernieError
      <$> (obj .: "message")
      <*> (obj .: "type")

data FicusMatchData = FicusMatchData
  { ficusMatchDataPattern :: Text
  , ficusMatchDataMatchString :: Text
  , ficusMatchDataScanType :: FicusScanType
  , ficusMatchDataName :: Text
  , ficusMatchDataStartByte :: Integer
  , ficusMatchDataEndByte :: Integer
  , ficusMatchDataStartLine :: Integer
  , ficusMatchDataEndLine :: Integer
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieMatchData where
  parseJSON = withObject "LernieMatchData" $ \obj ->
    LernieMatchData
      <$> (obj .: "pattern")
      <*> (obj .: "match_str")
      <*> (obj .: "scan_type")
      <*> (obj .: "name")
      <*> (obj .: "start_byte")
      <*> (obj .: "end_byte")
      <*> (obj .: "start_line")
      <*> (obj .: "end_line")

instance ToJSON LernieMatchData

data FicusResults = FicusResults
  { ficusResultsKeywordSearches :: [FicusMatch]
  , ficusResultsCustomLicenses :: [FicusMatch]
  , ficusResultsSourceUnit :: Maybe LicenseSourceUnit
  }
  deriving (Eq, Ord, Show, Generic)

data FicusMessages = FicusMessages
  { ficusMessageMatches :: [FicusMatch]
  , ficusMessageWarnings :: [FicusWarning]
  , ficusMessageErrors :: [FicusError]
  }
  deriving (Eq, Ord, Show, Generic)

instance Semigroup LernieMessages where
  LernieMessages w1 e1 m1 <> LernieMessages w2 e2 m2 = LernieMessages (w1 <> w2) (e1 <> e2) (m1 <> m2)

instance Monoid LernieMessages where
  mempty = LernieMessages [] [] []

data LernieMessage = LernieMessageLernieMatch LernieMatch | LernieMessageLernieWarning LernieWarning | LernieMessageLernieError LernieError
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieMessage where
  parseJSON (Object o) = do
    messageType <- o .: "type"
    case messageType of
      LernieMessageTypeWarning -> do
        Object d <- o .: "data"
        let message = d .: "message"
        let warningType = d .: "type"
        warning <- LernieWarning <$> warningType <*> message
        pure $ LernieMessageLernieWarning warning
      LernieMessageTypeError -> do
        Object d <- o .: "data"
        let errorType = d .: "type"
        let message = d .: "message"
        err <- LernieError <$> errorType <*> message
        pure $ LernieMessageLernieError err
      LernieMessageTypeMatch -> do
        Object d <- o .: "data"
        let path = d .: "path"
        let matches = d .: "matches"
        let contents = d .:? "contents"
        match <- LernieMatch <$> path <*> matches <*> contents
        pure $ LernieMessageLernieMatch match
  parseJSON _ = fail "Invalid schema for LernieMessage. It must be an object"
