{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Ficus.Types (
  FicusResults (..),
  FicusConfig (..),
  FicusRegex (..),
  FicusScanType (..),
  FicusMessage (..),
  FicusMessages (..),
  FicusMatch (..),
  FicusMatchData (..),
  FicusWarning (..),
  FicusError (..),
) where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), Value (Object), object, withObject, withText, (.:?))
import Data.Aeson qualified as A
import Data.Aeson.Types ((.:))
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Srclib.Types (LicenseSourceUnit)
import Types (LicenseScanPathFilters (..))

data FicusResults = FicusResults { ficusAnalysisId :: Int } deriving (Eq, Ord, Show, Generic)

-- FOSSA_API_TOKEN=foobar cargo run -- analyze --exclude ".git/**" --exclude "target/**" --endpoint http://localhost:3000 --locator "pip+flask$1.2.3" --set snippet-scanning:batch-length=123

data FicusConfig = FicusConfig
  { ficusRootDir :: Path Abs Dir
  , ficusFossaToken :: Text
  , ficusLocator :: Text
  , ficusGitignore :: Bool
  , ficusSkipHidden :: Bool
  , ficusAllExtensions :: Bool
  , ficusExcludes :: [Text]
  }
  deriving (Eq, Ord, Show, Generic)

data FicusRegex = FicusRegex
  { ficusPat :: Text
  , ficusName :: Text
  , ficusScanType :: FicusScanType
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON FicusRegex where
  toJSON FicusRegex{..} =
    object
      [ "pattern" .= toText ficusPat
      , "name" .= toText ficusName
      , "scan_type" .= toText ficusScanType
      ]

data FicusScanType = FicusCustomLicense | FicusKeywordSearch
  deriving (Eq, Ord, Show, Generic)

instance ToText FicusScanType where
  toText FicusCustomLicense = "CustomLicense"
  toText FicusKeywordSearch = "KeywordSearch"

instance ToJSON FicusScanType where
  toJSON = toJSON . toText

instance FromJSON FicusScanType

data FicusMessageType = FicusMessageTypeMatch | FicusMessageTypeError | FicusMessageTypeWarning
  deriving (Eq, Ord, Show, Generic)

instance ToText FicusMessageType where
  toText FicusMessageTypeMatch = "Match"
  toText FicusMessageTypeError = "Error"
  toText FicusMessageTypeWarning = "Warning"

instance ToJSON FicusMessageType where
  toJSON = toJSON . toText

instance FromJSON FicusMessageType where
  parseJSON = withText "FicusMessageType" $ \msg -> do
    case msg of
      "Match" -> pure FicusMessageTypeMatch
      "Error" -> pure FicusMessageTypeError
      "Warning" -> pure FicusMessageTypeWarning
      _ -> fail "invalid Ficus message type"

data FicusMatch = FicusMatch
  { ficusMatchPath :: Text
  , ficusMatchMatches :: [FicusMatchData]
  , ficusMatchContents :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FicusMatch where
  parseJSON = withObject "FicusMatch" $ \obj ->
    FicusMatch
      <$> (obj .: "path")
      <*> (obj .: "matches")
      <*> (obj .:? "contents")

data FicusWarning = FicusWarning
  { ficusWarningMessage :: Text
  , ficusWarningType :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FicusWarning where
  parseJSON = withObject "FicusWarning" $ \obj ->
    FicusWarning
      <$> (obj .: "message")
      <*> (obj .: "type")

data FicusError = FicusError
  { ficusErrorMessage :: Text
  , ficusErrorType :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FicusError where
  parseJSON = withObject "FicusError" $ \obj ->
    FicusError
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

instance FromJSON FicusMatchData where
  parseJSON = withObject "FicusMatchData" $ \obj ->
    FicusMatchData
      <$> (obj .: "pattern")
      <*> (obj .: "match_str")
      <*> (obj .: "scan_type")
      <*> (obj .: "name")
      <*> (obj .: "start_byte")
      <*> (obj .: "end_byte")
      <*> (obj .: "start_line")
      <*> (obj .: "end_line")

instance ToJSON FicusMatchData

data FicusMessages = FicusMessages
  { ficusMessageWarnings :: [FicusWarning]
  , ficusMessageErrors :: [FicusError]
  , ficusMessageMatches :: [FicusMatch]
  }
  deriving (Eq, Ord, Show, Generic)

instance Semigroup FicusMessages where
  FicusMessages w1 e1 m1 <> FicusMessages w2 e2 m2 = FicusMessages (w1 <> w2) (e1 <> e2) (m1 <> m2)

instance Monoid FicusMessages where
  mempty = FicusMessages [] [] []

data FicusMessage = FicusMessageFicusMatch FicusMatch | FicusMessageFicusWarning FicusWarning | FicusMessageFicusError FicusError
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FicusMessage where
  parseJSON (Object o) = do
    messageType <- o .: "type"
    case messageType of
      FicusMessageTypeWarning -> do
        Object d <- o .: "data"
        let message = d .: "message"
        let warningType = d .: "type"
        warning <- FicusWarning <$> warningType <*> message
        pure $ FicusMessageFicusWarning warning
      FicusMessageTypeError -> do
        Object d <- o .: "data"
        let errorType = d .: "type"
        let message = d .: "message"
        err <- FicusError <$> errorType <*> message
        pure $ FicusMessageFicusError err
      FicusMessageTypeMatch -> do
        Object d <- o .: "data"
        let path = d .: "path"
        let matches = d .: "matches"
        let contents = d .:? "contents"
        match <- FicusMatch <$> path <*> matches <*> contents
        pure $ FicusMessageFicusMatch match
  parseJSON _ = fail "Invalid schema for FicusMessage. It must be an object"
