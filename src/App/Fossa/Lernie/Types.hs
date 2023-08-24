{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Lernie.Types (
  LernieResults (..),
  LernieMatch (..),
  LernieWarning (..),
  LernieError (..),
  LernieMessage (..),
  LernieMessages (..),
  LernieConfig (..),
  LernieRegex (..),
  LernieMatchData (..),
  LernieScanType (..),
) where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), Value (Object), object, withObject, withText)
import Data.Aeson qualified as A
import Data.Aeson.Types ((.:))
import Data.List.NonEmpty (NonEmpty)
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Srclib.Types (LicenseSourceUnit)

data LernieConfig = LernieConfig
  { rootDir :: Path Abs Dir
  , regexes :: NonEmpty LernieRegex
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LernieConfig where
  toJSON LernieConfig{..} =
    object
      [ "root_dir" .= toText rootDir
      , "regexes" .= toJSON regexes
      ]

data LernieRegex = LernieRegex
  { pat :: Text
  , name :: Text
  , scanType :: LernieScanType
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LernieRegex where
  toJSON LernieRegex{..} =
    object
      [ "pattern" .= toText pat
      , "name" .= toText name
      , "scan_type" .= toText scanType
      ]

data LernieScanType = CustomLicense | KeywordSearch
  deriving (Eq, Ord, Show, Generic)

instance ToText LernieScanType where
  toText CustomLicense = "CustomLicense"
  toText KeywordSearch = "KeywordSearch"

instance ToJSON LernieScanType where
  toJSON = toJSON . toText

instance FromJSON LernieScanType

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

data LernieMatch = LernieMatch
  { lernieMatchPath :: Text
  , lernieMatchMatches :: [LernieMatchData]
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieMatch where
  parseJSON = withObject "LernieMatch" $ \obj ->
    LernieMatch
      <$> (obj .: "path")
      <*> (obj .: "matches")

data LernieWarning = LernieWarning
  { lernieWarningMessage :: Text
  , lernieWarningType :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieWarning where
  parseJSON = withObject "LernieWarning" $ \obj ->
    LernieWarning
      <$> (obj .: "message")
      <*> (obj .: "type")

data LernieError = LernieError
  { lernieErrorMessage :: Text
  , lernieErrorType :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LernieError where
  parseJSON = withObject "LernieError" $ \obj ->
    LernieError
      <$> (obj .: "message")
      <*> (obj .: "type")

data LernieMatchData = LernieMatchData
  { lernieMatchDataPattern :: Text
  , lernieMatchDataMatchString :: Text
  , lernieMatchDataScanType :: LernieScanType
  , lernieMatchDataName :: Text
  , lernieMatchDataStartByte :: Integer
  , lernieMatchDataEndByte :: Integer
  , lernieMatchDataStartLine :: Integer
  , lernieMatchDataEndLine :: Integer
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

data LernieResults = LernieResults
  { lernieResultsWarnings :: Maybe (NonEmpty LernieWarning)
  , lernieResultsErrors :: Maybe (NonEmpty LernieError)
  , lernieResultsSourceUnit :: Maybe LicenseSourceUnit
  , lernieResultsKeywordSearches :: Maybe (NonEmpty LernieMatch)
  , lernieResultsCustomLicenses :: Maybe (NonEmpty LernieMatch)
  }
  deriving (Eq, Ord, Show, Generic)

data LernieMessages = LernieMessages
  { lernieMessageWarnings :: [LernieWarning]
  , lernieMessageErrors :: [LernieError]
  , lernieMessageMatches :: [LernieMatch]
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
        let message_type = d .: "type"
        warning <- LernieWarning <$> message_type <*> message
        pure $ LernieMessageLernieWarning warning
      LernieMessageTypeError -> do
        Object d <- o .: "data"
        let message_type = d .: "type"
        let message = d .: "message"
        err <- LernieError <$> message_type <*> message
        pure $ LernieMessageLernieError err
      LernieMessageTypeMatch -> do
        Object d <- o .: "data"
        let path = d .: "path"
        let matches = d .: "matches"
        match <- LernieMatch <$> path <*> matches
        pure $ LernieMessageLernieMatch match
  parseJSON _ = fail "Invalid schema for LernieMessage. It must be an object"
