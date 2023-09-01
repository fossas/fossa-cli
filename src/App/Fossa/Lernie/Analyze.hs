{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Lernie.Analyze (
  analyzeWithLernie,
  -- Exported for testing
  singletonLernieMessage,
  lernieMessagesToLernieResults,
  grepOptionsToLernieConfig,
) where

import App.Fossa.Config.Analyze (GrepEntry (grepEntryMatchCriteria, grepEntryName), GrepOptions (..))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withLernieBinary)
import App.Fossa.Lernie.Types (
  LernieConfig (..),
  LernieError (..),
  LernieMatch (..),
  LernieMatchData (..),
  LernieMessage (..),
  LernieMessages (..),
  LernieRegex (..),
  LernieResults (..),
  LernieScanType (..),
  LernieWarning (..),
 )
import Control.Carrier.Diagnostics (Diagnostics, fatal, warn)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (decode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (fold, traverse_)
import Data.HashMap.Strict qualified as H
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToText (toText), decodeUtf8)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execCurrentDirStdinThrow)
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
      let lernieResults = lernieMessagesToLernieResults messages rootDir
      pure $ Just lernieResults
    Nothing -> pure Nothing

grepOptionsToLernieConfig :: Path Abs Dir -> GrepOptions -> Maybe LernieConfig
grepOptionsToLernieConfig rootDir grepOptions =
  case (customLicenseSearches <> keywordSearches) of
    [] -> Nothing
    res -> Just $ LernieConfig rootDir res
  where
    customLicenseSearches = map (grepEntryToLernieRegex CustomLicense) (customLicenseSearch grepOptions)
    keywordSearches = map (grepEntryToLernieRegex KeywordSearch) (keywordSearch grepOptions)

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
  result <- execCurrentDirStdinThrow (lernieCommand bin) lernieConfigJSON
  let messages = parseLernieJson result
  traverse_ (fatal . displayLernieError) $ lernieMessageErrors messages
  traverse_ (warn . displayLernieWarning) $ lernieMessageWarnings messages
  pure messages
  where
    displayLernieWarning :: LernieWarning -> Text
    displayLernieWarning LernieWarning{..} = lernieWarningType <> ": " <> lernieWarningMessage
    displayLernieError :: LernieError -> Text
    displayLernieError LernieError{..} = lernieErrorType <> ": " <> lernieErrorMessage

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
  fold messages
  where
    messageLines = BL.splitWith (== 10) out
    parsedLines = mapMaybe decode messageLines
    messages = map singletonLernieMessage parsedLines

lernieMessagesToLernieResults :: LernieMessages -> Path Abs Dir -> LernieResults
lernieMessagesToLernieResults LernieMessages{..} rootDir =
  LernieResults
    { lernieResultsKeywordSearches = keywordSearches
    , lernieResultsCustomLicenses = customLicenses
    , lernieResultsSourceUnit = sourceUnit
    }
  where
    keywordSearches = filterLernieMessages lernieMessageMatches KeywordSearch
    customLicenses = filterLernieMessages lernieMessageMatches CustomLicense
    sourceUnit = case NE.nonEmpty customLicenses of
      Nothing -> Nothing
      Just licenses -> lernieMatchToSourceUnit (NE.toList licenses) rootDir

-- add a LernieMessage to the corresponding entry of an empty LernieMessages
singletonLernieMessage :: LernieMessage -> LernieMessages
singletonLernieMessage message = case message of
  LernieMessageLernieMatch msg -> mempty{lernieMessageMatches = [msg]}
  LernieMessageLernieWarning msg -> mempty{lernieMessageWarnings = [msg]}
  LernieMessageLernieError msg -> mempty{lernieMessageErrors = [msg]}

-- filter lernie matches to a specific scan type, filtering out any lernie matches with no messages after they have been filtered out
filterLernieMessages :: [LernieMatch] -> LernieScanType -> [LernieMatch]
filterLernieMessages matches scanType =
  lernieMatchesWithoutEmpties
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
lernieMatchToSourceUnit :: [LernieMatch] -> Path Abs Dir -> Maybe LicenseSourceUnit
lernieMatchToSourceUnit matches rootDir =
  case NE.nonEmpty licenseUnits of
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
-- A licenseUnitMatchData can be built from a LernieMatch
-- A LicenseUnitData has many LicenseUnitMatchData. There will be one
-- LicenseUnitData per (path, title) pair, containing all of the
-- LicenseUnitMatchData for that pair
-- A LicenseUnit has many LicenseUnitData
licenseUnitsFromLernieMatches :: [LernieMatch] -> [LicenseUnit]
licenseUnitsFromLernieMatches matches = do
  let allLicenseUnitMatchData = concatMap lernieMatchToLicenseUnitMatchData matches
  let allLicenseUnitData = map createLicenseUnitDataSingles allLicenseUnitMatchData
  -- collectedLicenseUnitData has one LicenseUnitData per (path, title) pair, each one containing
  -- all of the LicenseUnitMatchData for that (path, title) pair
  let collectedLicenseUnitData = HashMap.fromListWith (<>) allLicenseUnitData
  -- Now that we have all of the LicenseUnitData, we need to create LicenseUnits for them.
  -- LicenseUnits do not have paths on them, so we group them up by title
  let allLicenseUnits = map createLicenseUnitSingles $ HashMap.toList collectedLicenseUnitData
  let licenseUnitsByTitle = map (\((_, title), lu) -> (title, lu)) allLicenseUnits
  H.elems $ HashMap.fromListWith (<>) licenseUnitsByTitle

-- Create a list with keys of (path, title) and a value of a single LicenseUnitMatchData
lernieMatchToLicenseUnitMatchData :: LernieMatch -> [((CustomLicensePath, CustomLicenseTitle), LicenseUnitMatchData)]
lernieMatchToLicenseUnitMatchData LernieMatch{..} =
  map (createLicenseUnitMatchData $ CustomLicensePath lernieMatchPath) lernieMatchMatches

-- Given a path and a LernieMatchData, create a single LicenseUnitMatchData
createLicenseUnitMatchData :: CustomLicensePath -> LernieMatchData -> ((CustomLicensePath, CustomLicenseTitle), LicenseUnitMatchData)
createLicenseUnitMatchData path LernieMatchData{..} =
  ((path, title), licenseUnitMatchData)
  where
    title = CustomLicenseTitle lernieMatchDataName
    licenseUnitMatchData =
      LicenseUnitMatchData
        { licenseUnitMatchDataMatchString = Just lernieMatchDataMatchString
        , licenseUnitMatchDataLocation = lernieMatchDataStartByte
        , licenseUnitMatchDataLength = lernieMatchDataEndByte - lernieMatchDataStartByte
        , licenseUnitMatchDataIndex = 1
        , licenseUnitDataStartLine = lernieMatchDataStartLine
        , licenseUnitDataEndLine = lernieMatchDataEndLine
        }

createLicenseUnitDataSingles :: ((CustomLicensePath, CustomLicenseTitle), LicenseUnitMatchData) -> ((CustomLicensePath, CustomLicenseTitle), LicenseUnitData)
createLicenseUnitDataSingles ((path, title), licenseUnitMatchData) =
  ((path, title), newLicenseUnitData)
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

createLicenseUnitSingles :: ((CustomLicensePath, CustomLicenseTitle), LicenseUnitData) -> ((CustomLicensePath, CustomLicenseTitle), LicenseUnit)
createLicenseUnitSingles ((path, title), licenseUnitData) =
  ((path, title), lu)
  where
    lu =
      LicenseUnit
        { licenseUnitName = "custom-license"
        , licenseUnitType = "LicenseUnit"
        , licenseUnitTitle = Just $ unCustomLicenseTitle title
        , licenseUnitDir = ""
        , licenseUnitFiles = NE.singleton (unCustomLicensePath path)
        , licenseUnitData = NE.singleton licenseUnitData
        , licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Just $ "custom license search " <> unCustomLicenseTitle title}
        }
