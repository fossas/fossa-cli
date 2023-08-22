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
import Data.HashMap.Lazy (foldrWithKey)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Data.Hashable (Hashable)
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

newtype CustomLicensePath = CustomLicensePath {unCustomLicensePath :: Text}
  deriving (Eq, Ord, Show, Hashable)
newtype CustomLicenseTitle = CustomLicenseTitle {unCustomLicenseTitle :: Text}
  deriving (Eq, Ord, Show, Hashable)

-- | scan rootDir with Lernie, using the given GrepOptions. This is the main entry point to this module
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

-- Run Lernie, passing "--config -" as its arg so that it gets its config from STDIN
lernieCommand :: BinaryPaths -> Command
lernieCommand bin =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = ["--config", "-"]
    , cmdAllowErr = Never
    }

-- Parse Lernie's NDJson output by splitting on newlines (character 10) and
-- then decoding each line
parseLernieJson :: BL.ByteString -> LernieMessages
parseLernieJson out =
  foldr addLernieMessage emptyLernieMessages parsedLines
  where
    messageLines = BL.splitWith (== 10) out
    parsedLines :: [LernieMessage]
    parsedLines = mapMaybe decode messageLines

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

-- add a LernieMessage to the corresponding entry in LernieMessages
addLernieMessage :: LernieMessage -> LernieMessages -> LernieMessages
addLernieMessage message existing = case message of
  LernieMessageLernieMatch msg -> existing{lernieMessageMatches = msg : lernieMessageMatches existing}
  LernieMessageLernieWarning msg -> existing{lernieMessageWarnings = msg : lernieMessageWarnings existing}
  LernieMessageLernieError msg -> existing{lernieMessageErrors = msg : lernieMessageErrors existing}

-- filter lernie matches to a specific scan type, filtering out any lernie matches with no messages after they have been filtered out
filterLernieMessages :: [LernieMatch] -> LernieScanType -> Maybe (NonEmpty LernieMatch)
filterLernieMessages matches scanType =
  NE.nonEmpty lernieMatchesWithoutEmpties
  where
    byScanType :: LernieMatchData -> Bool
    byScanType m = scanType == lernieMatchDataScanType m
    lernieMatchesFilteredToScanType = map (\lm -> LernieMatch (lernieMatchPath lm) (filter byScanType $ lernieMatchMatches lm)) matches
    lernieMatchesWithoutEmpties = filter (not . null . lernieMatchMatches) lernieMatchesFilteredToScanType

-- Convert a list of lernie matches into a LicenseSourceUnit
-- The hard part is constructing the licenseSourceUnitLicenseUnits. This is an array of LicenseUnit, one per license found.
-- A LicenseUnit contains info about multiple files. Each file shows up in the Files attribute and in the
-- same position in the Data attribute.
-- So this function takes all of the Lernie Messages, which contain information about a single match in a single file,
-- and flips it around to get a list of licenses and the files they apply to.
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

-- Create LicenseUnits from the LernieMatches. All LicenseUnits will have a license ID of "custom-license".
-- There will be one LicenseUnit per custom-license title, and each LicenseUnit can contain results from multiple files.
licenseUnitsFromLernieMatches :: NonEmpty LernieMatch -> Maybe (NonEmpty LicenseUnit)
licenseUnitsFromLernieMatches matches = do
  let allLicenseUnitMatchData = createAllLicenseUnitMatchData matches
  let allLicenseUnits = foldrWithKey createLicenseUnitsFromMatchDatas H.empty allLicenseUnitMatchData
  NE.nonEmpty $ H.elems allLicenseUnits

createAllLicenseUnitMatchData :: NonEmpty LernieMatch -> HashMap (CustomLicensePath, CustomLicenseTitle) (NonEmpty LicenseUnitMatchData)
createAllLicenseUnitMatchData = foldr addLernieMatchToMatchData H.empty

-- Add all of the matches in a LernieMatch to the existing match data
addLernieMatchToMatchData :: LernieMatch -> HashMap (CustomLicensePath, CustomLicenseTitle) (NonEmpty LicenseUnitMatchData) -> HashMap (CustomLicensePath, CustomLicenseTitle) (NonEmpty LicenseUnitMatchData)
addLernieMatchToMatchData lernieMatch existingMatches =
  foldr (addLernieMatchDataToMatchData (CustomLicensePath $ lernieMatchPath lernieMatch)) existingMatches (lernieMatchMatches lernieMatch)

-- Add a single LernieMatchData to the existing match data
addLernieMatchDataToMatchData :: CustomLicensePath -> LernieMatchData -> HashMap (CustomLicensePath, CustomLicenseTitle) (NonEmpty LicenseUnitMatchData) -> HashMap (CustomLicensePath, CustomLicenseTitle) (NonEmpty LicenseUnitMatchData)
addLernieMatchDataToMatchData path lernieMatchData existingMatches =
  H.insert (path, title) newMatchDatas existingMatches
  where
    title = CustomLicenseTitle $ lernieMatchDataName lernieMatchData
    startByte = lernieMatchDataStartByte lernieMatchData
    endByte = lernieMatchDataEndByte lernieMatchData
    newMatchData =
      LicenseUnitMatchData
        { licenseUnitMatchDataMatchString = Just $ lernieMatchDataMatchString lernieMatchData
        , licenseUnitMatchDataLocation = startByte
        , licenseUnitMatchDataLength = endByte - startByte
        , licenseUnitMatchDataIndex = 1
        , licenseUnitDataStartLine = lernieMatchDataStartLine lernieMatchData
        , licenseUnitDataEndLine = lernieMatchDataEndLine lernieMatchData
        }
    newMatchDatas = case H.lookup (path, title) existingMatches of
      Nothing -> NE.singleton newMatchData
      Just existing -> NE.cons newMatchData existing

-- Take a list of LicenseUnitMatchData and their path and title and create the resulting license units
createLicenseUnitsFromMatchDatas :: (CustomLicensePath, CustomLicenseTitle) -> NonEmpty LicenseUnitMatchData -> HashMap CustomLicenseTitle LicenseUnit -> HashMap CustomLicenseTitle LicenseUnit
createLicenseUnitsFromMatchDatas (path, title) licenseUnits existingUnits = foldr (createLicenseUnitsFromMatchData path title) existingUnits licenseUnits

-- Given a LicenseUnitMatchData, its path and its title, add it to the license already existing units
-- If a license unit with that title already exists, then add it to that. Otherwise create a new one.
createLicenseUnitsFromMatchData :: CustomLicensePath -> CustomLicenseTitle -> LicenseUnitMatchData -> HashMap CustomLicenseTitle LicenseUnit -> HashMap CustomLicenseTitle LicenseUnit
createLicenseUnitsFromMatchData path title licenseUnitMatchData existingUnits =
  H.insert title newLicenseUnit existingUnits
  where
    newLicenseUnitData =
      LicenseUnitData
        { licenseUnitDataPath = unCustomLicensePath path
        , licenseUnitDataCopyright = Nothing
        , licenseUnitDataThemisVersion = ""
        , licenseUnitDataMatchData = Just $ NE.singleton licenseUnitMatchData
        , licenseUnitDataCopyrights = Nothing
        , licenseUnitDataContents = Nothing
        }
    newLicenseUnit = case H.lookup title existingUnits of
      Nothing -> do
        LicenseUnit
          { licenseUnitName = "custom-license"
          , licenseUnitType = "LicenseUnit"
          , licenseUnitTitle = Just $ unCustomLicenseTitle title
          , licenseUnitDir = ""
          , licenseUnitFiles = (unCustomLicensePath path) NE.:| []
          , licenseUnitData = newLicenseUnitData NE.:| []
          , licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}
          }
      Just existingUnit ->
        existingUnit
          { licenseUnitData = NE.cons newLicenseUnitData (licenseUnitData existingUnit)
          , licenseUnitFiles = NE.nub $ NE.cons (unCustomLicensePath path) (licenseUnitFiles existingUnit)
          }
