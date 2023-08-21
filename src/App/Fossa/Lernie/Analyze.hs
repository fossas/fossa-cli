{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Lernie.Analyze (
  analyzeWithLernie,
  addLernieMessage,
  lernieMessagesToLernieResults,
  grepOptionsToLernieConfig,
) where

import App.Fossa.Config.Analyze (GrepEntry (grepEntryMatchCriteria, grepEntryName), GrepOptions (..))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withLernieBinary)
import App.Fossa.Lernie.Types (
  LernieConfig (..),
  LernieMatch (..),
  LernieMatchData (..),
  LernieMessage (..),
  LernieMessages (..),
  LernieRegex (..),
  LernieResults (..),
  LernieScanType (..),
  emptyLernieMessages,
 )
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (decode)
import Data.Aeson qualified as Aeson
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
import Path (Abs, Dir, Path)
import Srclib.Types (LicenseScanType (..), LicenseSourceUnit (..), LicenseUnit (..), LicenseUnitData (..), LicenseUnitInfo (..), LicenseUnitMatchData (..))

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
    sourceUnit = case customLicenses of
      Nothing -> Nothing
      Just licenses -> lernieMatchToSourceUnit licenses rootDir

-- | filter lernie matches to a specific scan type, filtering out any lernie matches with no messages after they have been filtered out
filterLernieMessages :: [LernieMatch] -> LernieScanType -> Maybe (NonEmpty LernieMatch)
filterLernieMessages matches scanType =
  NE.nonEmpty lernieMatchesWithoutEmpties
  where
    byScanType :: LernieMatchData -> Bool
    byScanType m = scanType == lernieMatchDataScanType m
    lernieMatchesFilteredToScanType = map (\lm -> LernieMatch (lernieMatchPath lm) (filter byScanType $ lernieMatchMatches lm)) matches
    lernieMatchesWithoutEmpties = filter (not . null . lernieMatchMatches) lernieMatchesFilteredToScanType

-- | convert a list of lernie matches (of type CustomLicense, typically) into a LicenseSourceUnit
lernieMatchToSourceUnit :: NonEmpty LernieMatch -> Path Abs Dir -> Maybe LicenseSourceUnit
lernieMatchToSourceUnit matches rootDir =
  case licenseUnits of
    Just units ->
      Just
        LicenseSourceUnit
          { licenseSourceUnitName = toText rootDir
          , licenseSourceUnitType = CliLicenseScanned
          , licenseSourceUnitLicenseUnits = units
          }
    Nothing -> Nothing
  where
    licenseUnits = licenseUnitsFromLernieMatches matches

-- Create LicenseUnits from the LernieMatches. There will be one LicenseUnit per custom-license name
licenseUnitsFromLernieMatches :: NonEmpty LernieMatch -> Maybe (NonEmpty LicenseUnit)
licenseUnitsFromLernieMatches matches =
  NE.nonEmpty $ H.elems $ foldr addMatchesToLicenseUnits H.empty matches

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
        , licenseUnitMatchDataLength = endByte - startByte
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
          { licenseUnitName = "custom-license"
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

addLernieMessage :: LernieMessage -> LernieMessages -> LernieMessages
addLernieMessage message existing = case message of
  LernieMessageLernieMatch msg -> existing{lernieMessageMatches = msg : lernieMessageMatches existing}
  LernieMessageLernieWarning msg -> existing{lernieMessageWarnings = msg : lernieMessageWarnings existing}
  LernieMessageLernieError msg -> existing{lernieMessageErrors = msg : lernieMessageErrors existing}

analyzeWithLernie ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  GrepOptions ->
  m (Maybe LernieResults)
analyzeWithLernie rootDir _maybeApiOpts grepOptions = do
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

grepEntryToLernieRegex :: LernieScanType -> GrepEntry -> LernieRegex
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
