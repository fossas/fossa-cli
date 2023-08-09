{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Grep (
  analyzeWithGrep,
  LernieResults (..),
) where

import App.Fossa.Config.Analyze (GrepEntry (grepEntryMatchCriteria, grepEntryName), GrepOptions (..))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withLernieBinary)
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), Value (Object), decode, object, withObject, withText)
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Aeson.Types ((.:))
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Extra ((<$$>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToText (toText), decodeUtf8)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execThrow'')
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Srclib.Types (LicenseScanType (..), LicenseSourceUnit (..), LicenseUnit (..), LicenseUnitData (..), LicenseUnitInfo (..), LicenseUnitMatchData (..))

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
  { pattern :: Text
  , name :: Text
  , scanType :: GrepScanType
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LernieRegex where
  toJSON LernieRegex{..} =
    object
      [ "pattern" .= toText pattern
      , "name" .= toText name
      , "scan_type" .= toText scanType
      ]

data GrepScanType = CustomLicense | KeywordSearch
  deriving (Eq, Ord, Show, Generic)

instance ToText GrepScanType where
  toText CustomLicense = "CustomLicense"
  toText KeywordSearch = "KeywordSearch"

instance ToJSON GrepScanType where
  toJSON = toJSON . toText

instance FromJSON GrepScanType

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
      _ -> pure LernieMessageTypeWarning

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
  , lernieMatchDataScanType :: GrepScanType
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

lernieMessagesToLernieResults :: LernieMessages -> Path Abs Dir -> LernieResults
lernieMessagesToLernieResults LernieMessages{..} rootDir =
  LernieResults
    { lernieResultsWarnings = warnings
    , lernieResultsErrors = errors
    , lernieResultsKeywordSearches = keywordSearches
    , lernieResultsCustomLicenses = customLicenses
    , lernieResultsSourceUnit = sourceUnit
    }
  where
    warnings = NE.nonEmpty lernieMessageWarnings
    errors = NE.nonEmpty lernieMessageErrors
    keywordSearches = filterLernieMessages lernieMessageMatches KeywordSearch
    customLicenses = filterLernieMessages lernieMessageMatches CustomLicense
    -- TODO: start with customLicenses and convert it into a sourceUnit, flipping around the files and the license names
    -- We should have one LicenseUnit per lernieMatchDataName, I think
    sourceUnit = case customLicenses of
      Nothing -> Nothing
      Just licenses -> Just $ lernieMatchToSourceUnit licenses rootDir

-- | filter lernie matches to a specific scan type, filtering out any lernie matches with no messages after they have been filtered out
filterLernieMessages :: [LernieMatch] -> GrepScanType -> Maybe (NonEmpty LernieMatch)
filterLernieMessages matches scanType =
  NE.nonEmpty lernieMatchesWithoutEmpties
  where
    byScanType :: LernieMatchData -> Bool
    byScanType m = scanType == lernieMatchDataScanType m
    lernieMatchesFilteredToScanType = map (\lm -> LernieMatch (lernieMatchPath lm) (filter byScanType $ lernieMatchMatches lm)) matches
    lernieMatchesWithoutEmpties = filter (not . null . lernieMatchMatches) lernieMatchesFilteredToScanType

-- | convert a list of lernie matches (of type CustomLicense, typically) into a LicenseSourceUnit
lernieMatchToSourceUnit :: NonEmpty LernieMatch -> Path Abs Dir -> LicenseSourceUnit
lernieMatchToSourceUnit matches rootDir =
  LicenseSourceUnit
    { licenseSourceUnitName = toText rootDir
    , licenseSourceUnitType = CliLicenseScanned
    , licenseSourceUnitLicenseUnits = licenseUnits
    }
  where
    licenseUnits = licenseUnitsFromLernieMatches matches

-- Create LicenseUnits from the LernieMatches. There will be one LicenseUnit per custom-license name
licenseUnitsFromLernieMatches :: NonEmpty LernieMatch -> NonEmpty LicenseUnit
licenseUnitsFromLernieMatches matches =
  NE.fromList $ H.elems $ foldr addMatchesToLicenseUnits H.empty matches

-- Add a lernieMatch to the licenseUnits
addMatchesToLicenseUnits :: LernieMatch -> HashMap Text LicenseUnit -> HashMap Text LicenseUnit
addMatchesToLicenseUnits match existingUnits =
  foldr (addMatchDataToLicenseUnits $ lernieMatchPath match) existingUnits $ lernieMatchMatches match

-- Add a LernieMatchData to the existing licenseUnits, creating a new LicenseUnit if one with that title does not already exist
addMatchDataToLicenseUnits :: Text -> LernieMatchData -> HashMap Text LicenseUnit -> HashMap Text LicenseUnit
addMatchDataToLicenseUnits path matchData existingUnits =
  H.insert name newUnit existingUnits
  where
    name = lernieMatchDataName matchData
    startByte = lernieMatchDataStartByte matchData
    endByte = lernieMatchDataEndByte matchData
    licenseUnitMatchData =
      LicenseUnitMatchData
        { licenseUnitMatchDataMatchString = Just $ lernieMatchDataMatchString matchData
        , licenseUnitMatchDataLocation = startByte
        , licenseUnitMatchDataLength = startByte + endByte
        , licenseUnitMatchDataIndex = 1
        , licenseUnitDataStartLine = lernieMatchDataStartLine matchData
        , licenseUnitDataEndLine = lernieMatchDataEndLine matchData
        }
    -- TODO: What happens if we find the same custom license twice in one file?
    newUnitData =
      LicenseUnitData
        { licenseUnitDataPath = path
        , licenseUnitDataCopyright = Nothing
        , licenseUnitDataThemisVersion = ""
        , licenseUnitDataMatchData = Just $ NE.singleton licenseUnitMatchData
        , licenseUnitDataCopyrights = Nothing
        , licenseUnitDataContents = Nothing
        }
    newUnit = case H.lookup name existingUnits of
      Nothing ->
        LicenseUnit
          { licenseUnitName = "customlicense"
          , licenseUnitType = "LicenseUnit"
          , licenseUnitTitle = Just name
          , licenseUnitDir = ""
          , licenseUnitFiles = NE.singleton path
          , licenseUnitData = NE.singleton newUnitData
          , licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}
          }
      Just existingUnit -> do
        existingUnit
          { licenseUnitFiles = NE.cons path $ licenseUnitFiles existingUnit
          , licenseUnitData = NE.cons newUnitData $ licenseUnitData existingUnit
          }

data LernieMessages = LernieMessages
  { lernieMessageWarnings :: [LernieWarning]
  , lernieMessageErrors :: [LernieError]
  , lernieMessageMatches :: [LernieMatch]
  }
  deriving (Eq, Ord, Show, Generic)

data LernieMessage = LernieMessageLernieMatch LernieMatch | LernieMessageLernieWarning LernieWarning | LernieMessageLernieError LernieError
  deriving (Eq, Ord, Show, Generic)

emptyLernieMessages :: LernieMessages
emptyLernieMessages = LernieMessages [] [] []

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

addLernieMessage :: LernieMessage -> LernieMessages -> LernieMessages
addLernieMessage message existing = case message of
  LernieMessageLernieMatch msg -> existing{lernieMessageMatches = msg : lernieMessageMatches existing}
  LernieMessageLernieWarning msg -> existing{lernieMessageWarnings = msg : lernieMessageWarnings existing}
  LernieMessageLernieError msg -> existing{lernieMessageErrors = msg : lernieMessageErrors existing}

analyzeWithGrep ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  GrepOptions ->
  m (Maybe LernieResults)
analyzeWithGrep rootDir _maybeApiOpts grepOptions = do
  -- TODO: convert grepOptions to lernieOpts
  let maybeLernieConfig = grepOptionsToLernieConfig rootDir grepOptions
  case maybeLernieConfig of
    Just (lernieConfig) -> do
      messages <- runLernie lernieConfig
      pure $ Just $ lernieMessagesToLernieResults messages rootDir
    Nothing -> pure Nothing

grepOptionsToLernieConfig :: Path Abs Dir -> GrepOptions -> Maybe LernieConfig
grepOptionsToLernieConfig rootDir grepOptions =
  case regexes of
    Nothing -> Nothing
    Just res -> Just $ LernieConfig rootDir res
  where
    customLicenseSearches = grepEntryToLernieRegex CustomLicense <$$> customLicenseSearch grepOptions
    keywordSearches = grepEntryToLernieRegex KeywordSearch <$$> keywordSearch grepOptions

    regexes = case (customLicenseSearches, keywordSearches) of
      (Nothing, Just grepEntries) -> Just grepEntries
      (Just grepEntries, Nothing) -> Just grepEntries
      (Just customLicenseEntries, Just keywordEntries) -> Just $ customLicenseEntries <> keywordEntries
      (Nothing, Nothing) -> Nothing

grepEntryToLernieRegex :: GrepScanType -> GrepEntry -> LernieRegex
grepEntryToLernieRegex scanType grepEntry =
  LernieRegex (grepEntryMatchCriteria grepEntry) (grepEntryName grepEntry) scanType

runLernie ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  LernieConfig ->
  m LernieMessages
runLernie lernieConfig = withLernieBinary $ \bin -> do
  let lernieConfigJSON = decodeUtf8 $ Aeson.encode lernieConfig
  result <- execThrow'' (lernieCommand bin) lernieConfigJSON
  pure $ parseLernieJson result

parseLernieJson :: BL.ByteString -> LernieMessages
parseLernieJson out =
  foldr addLernieMessage emptyLernieMessages parsedLines
  where
    messageLines = BL.splitWith (== 10) out
    parsedLines :: [LernieMessage]
    parsedLines = mapMaybe decode messageLines

lernieCommand :: BinaryPaths -> Command
lernieCommand bin =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = ["--config", "-"]
    , cmdAllowErr = Never
    }
