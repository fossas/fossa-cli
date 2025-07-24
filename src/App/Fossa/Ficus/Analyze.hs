{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Ficus.Analyze (
  analyzeWithFicus,
  -- Exported for use in hubble
  analyzeWithFicusMain,
  -- Exported for testing
  analyzeWithFicusWithOrgInfo,
  singletonFicusMessage,
  ficusMessagesToFicusResults,
  grepOptionsToFicusConfig,
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withFicusBinary)
import App.Fossa.Ficus.Types (
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
 )
import App.Fossa.Lernie.Types (
  GrepEntry (..),
  GrepOptions (..),
  OrgWideCustomLicenseConfigPolicy (..),
 )
import App.Types (FileUpload (..))
import Control.Carrier.Debug (Debug)
import Control.Carrier.Diagnostics (Diagnostics, fatal, warn)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.Telemetry.Types
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.Telemetry (Telemetry, trackUsage)
import Control.Monad (join, unless)
import Data.Aeson (decode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (fold, traverse_)
import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToText (toText), decodeUtf8)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execCurrentDirStdinThrow)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts, Organization (..), orgFileUpload)
import Path (Abs, Dir, File, Path)
import Srclib.Types (LicenseScanType (..), LicenseSourceUnit (..), LicenseUnit (..), LicenseUnitData (..), LicenseUnitInfo (..), LicenseUnitMatchData (..))
import Types (LicenseScanPathFilters)

newtype CustomLicensePath = CustomLicensePath {unCustomLicensePath :: Text}
  deriving (Eq, Ord, Show, Hashable)
newtype CustomLicenseTitle = CustomLicenseTitle {unCustomLicenseTitle :: Text}
  deriving (Eq, Ord, Show, Hashable)

-- | scan rootDir with Ficus, using the given GrepOptions. This is the main entry point to this module
analyzeWithFicus ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Debug sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  Path Abs Dir ->
  Maybe ApiOpts ->
  GrepOptions ->
  Maybe LicenseScanPathFilters ->
  m (Maybe FicusResults)
analyzeWithFicus rootDir maybeApiOpts grepOptions filters = do
  case (maybeApiOpts, orgWideCustomLicenseScanConfigPolicy grepOptions) of
    (_, Ignore) -> analyzeWithFicusMain rootDir grepOptions filters
    (Nothing, Use) -> analyzeWithFicusMain rootDir grepOptions filters
    (Just apiOpts, Use) -> runFossaApiClient apiOpts $ analyzeWithFicusWithOrgInfo rootDir grepOptions filters

analyzeWithFicusMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  Path Abs Dir ->
  GrepOptions ->
  Maybe LicenseScanPathFilters ->
  m (Maybe FicusResults)
analyzeWithFicusMain rootDir grepOptions filters = do
  let maybeFicusConfig = grepOptionsToFicusConfig rootDir grepOptions filters
  case maybeFicusConfig of
    Just ficusConfig -> do
      unless (null $ customLicenseSearch grepOptions) $ trackUsage CustomLicenseSearchUsage
      unless (null $ keywordSearch grepOptions) $ trackUsage ExperimentalKeywordSearchUsage
      messages <- runFicus ficusConfig (configFilePath grepOptions)
      let ficusResults = ficusMessagesToFicusResults messages rootDir
      pure $ Just ficusResults
    Nothing -> pure Nothing

grepOptionsToFicusConfig :: Path Abs Dir -> GrepOptions -> Maybe LicenseScanPathFilters -> FileUpload -> Maybe FicusConfig
grepOptionsToFicusConfig rootDir grepOptions filters uploadKind =
  case (customLicenseSearches <> keywordSearches) of
    [] -> Nothing
    res -> Just $ FicusConfig
      { ficusRootDir = rootDir
      , ficusRegexes = res
      , ficusLicenseScanPathFilters = filters
      , ficusFullFiles = case uploadKind of
          FileUploadMatchData -> False
          FileUploadFullContent -> True
      }
  where
    customLicenseSearches = map (grepEntryToFicusRegex FicusCustomLicense) (customLicenseSearch grepOptions)
    keywordSearches = map (grepEntryToFicusRegex FicusKeywordSearch) (keywordSearch grepOptions)

grepEntryToFicusRegex :: FicusScanType -> GrepEntry -> FicusRegex
grepEntryToFicusRegex scanType grepEntry =
  FicusRegex
    { ficusPat = grepEntryMatchCriteria grepEntry
    , ficusName = grepEntryName grepEntry
    , ficusScanType = scanType
    }

runFicus ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  FicusConfig ->
  Maybe (Path Abs File) ->
  m FicusMessages
runFicus ficusConfig configFilePath = withFicusBinary $ \bin -> do
  let ficusConfigJSON = decodeUtf8 $ Aeson.encode ficusConfig
  result <- execCurrentDirStdinThrow (ficusCommand bin) ficusConfigJSON
  let messages = parseFicusJson result configFilePath
  traverse_ (fatal . displayFicusError) $ ficusMessageErrors messages
  traverse_ (warn . displayFicusWarning) $ ficusMessageWarnings messages
  pure messages
  where
    displayFicusWarning :: FicusWarning -> Text
    displayFicusWarning FicusWarning{..} = ficusWarningType <> ": " <> ficusWarningMessage
    displayFicusError :: FicusError -> Text
    displayFicusError FicusError{..} = ficusErrorType <> ": " <> ficusErrorMessage

-- Run Ficus, passing "x-walk --config -" as its arg so that it gets its config from STDIN
ficusCommand :: BinaryPaths -> Command
ficusCommand bin =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = ["x-walk", "--config", "-"]
    , cmdAllowErr = Never
    }

-- Parse Ficus's NDJson output by splitting on newlines (character 10) and
-- then decoding each line
parseFicusJson :: BL.ByteString -> Maybe (Path Abs File) -> FicusMessages
parseFicusJson out configFilePath =
  fold messages
  where
    messageLines = BL.splitWith (== 10) out
    -- Once Ficus supports file filtering we'll do this by passing the config file path into Ficus
    -- and Ficus will skip scanning the config file. But for now let's just filter post-scan
    parsedLines = filter (notConfigFile configFilePath) $ mapMaybe decode messageLines
    messages = map singletonFicusMessage parsedLines

notConfigFile :: Maybe (Path Abs File) -> FicusMessage -> Bool
notConfigFile (Just configFilePath) (FicusMessageFicusMatch ficusMessage) = ficusMatchPath ficusMessage /= toText configFilePath
notConfigFile _ _ = True

ficusMessagesToFicusResults :: FicusMessages -> Path Abs Dir -> FicusResults
ficusMessagesToFicusResults FicusMessages{..} rootDir =
  FicusResults
    { ficusResultsKeywordSearches = keywordSearches
    , ficusResultsCustomLicenses = customLicenses
    , ficusResultsSourceUnit = sourceUnit
    }
  where
    keywordSearches = filterFicusMessages ficusMessageMatches FicusKeywordSearch
    customLicenses = filterFicusMessages ficusMessageMatches FicusCustomLicense
    sourceUnit = case NE.nonEmpty customLicenses of
      Nothing -> Nothing
      Just licenses -> ficusMatchToSourceUnit (NE.toList licenses) rootDir

-- add a FicusMessage to the corresponding entry of an empty FicusMessages
singletonFicusMessage :: FicusMessage -> FicusMessages
singletonFicusMessage message = case message of
  FicusMessageFicusMatch msg -> mempty{ficusMessageMatches = [msg]}
  FicusMessageFicusWarning msg -> mempty{ficusMessageWarnings = [msg]}
  FicusMessageFicusError msg -> mempty{ficusMessageErrors = [msg]}

-- filter ficus matches to a specific scan type, filtering out any ficus matches with no messages after they have been filtered out
filterFicusMessages :: [FicusMatch] -> FicusScanType -> [FicusMatch]
filterFicusMessages matches scanType =
  ficusMatchesWithoutEmpties
  where
    byScanType :: FicusMatchData -> Bool
    byScanType m = scanType == ficusMatchDataScanType m
    ficusMatchesFilteredToScanType = map (\fm -> FicusMatch (ficusMatchPath fm) (filter byScanType $ ficusMatchMatches fm) (ficusMatchContents fm)) matches
    ficusMatchesWithoutEmpties = filter (not . null . ficusMatchMatches) ficusMatchesFilteredToScanType

-- Convert a list of ficus matches into a LicenseSourceUnit
-- The hard part is constructing the licenseSourceUnitLicenseUnits. This is an array of LicenseUnit, one per license found.
-- A LicenseUnit contains info about multiple files. Each file shows up in the Files attribute and in the
-- same position in the Data attribute.
-- So this function takes all of the Ficus Messages, which contain information about a single match in a single file,
-- and flips it around to get a list of licenses and the files they apply to.
ficusMatchToSourceUnit :: [FicusMatch] -> Path Abs Dir -> Maybe LicenseSourceUnit
ficusMatchToSourceUnit matches rootDir =
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
    licenseUnits = licenseUnitsFromFicusMatches matches

-- Create LicenseUnits from the FicusMatches. All LicenseUnits will have a license ID of "custom-license".
-- There will be one LicenseUnit per custom-license title, and each LicenseUnit can contain results from multiple files.
-- A licenseUnitMatchData can be built from a FicusMatch
-- A LicenseUnitData has many LicenseUnitMatchData. There will be one
-- LicenseUnitData per (path, title) pair, containing all of the
-- LicenseUnitMatchData for that pair
-- A LicenseUnit has many LicenseUnitData
licenseUnitsFromFicusMatches :: [FicusMatch] -> [LicenseUnit]
licenseUnitsFromFicusMatches matches = do
  let allLicenseUnitMatchData = concatMap ficusMatchToLicenseUnitMatchData matches
  let fileContents = HashMap.fromList $ map getContentsFromFicusMatch matches
  let allLicenseUnitData = map (createLicenseUnitDataSingles fileContents) allLicenseUnitMatchData
  -- collectedLicenseUnitData has one LicenseUnitData per (path, title) pair, each one containing
  -- all of the LicenseUnitMatchData for that (path, title) pair
  let collectedLicenseUnitData = HashMap.fromListWith (<>) allLicenseUnitData
  -- Now that we have all of the LicenseUnitData, we need to create LicenseUnits for them.
  -- LicenseUnits do not have paths on them, so we group them up by title
  let allLicenseUnits = map createLicenseUnitSingles $ HashMap.toList collectedLicenseUnitData
  let licenseUnitsByTitle = map (\((_, title), lu) -> (title, lu)) allLicenseUnits
  H.elems $ HashMap.fromListWith (<>) licenseUnitsByTitle

getContentsFromFicusMatch :: FicusMatch -> (CustomLicensePath, Maybe Text)
getContentsFromFicusMatch FicusMatch{..} = (CustomLicensePath ficusMatchPath, ficusMatchContents)

-- Create a list with keys of (path, title) and a value of a single LicenseUnitMatchData
ficusMatchToLicenseUnitMatchData :: FicusMatch -> [((CustomLicensePath, CustomLicenseTitle), LicenseUnitMatchData)]
ficusMatchToLicenseUnitMatchData FicusMatch{..} =
  map (createLicenseUnitMatchData $ CustomLicensePath ficusMatchPath) ficusMatchMatches

-- Given a path and a FicusMatchData, create a single LicenseUnitMatchData
createLicenseUnitMatchData :: CustomLicensePath -> FicusMatchData -> ((CustomLicensePath, CustomLicenseTitle), LicenseUnitMatchData)
createLicenseUnitMatchData path FicusMatchData{..} =
  ((path, title), licenseUnitMatchData)
  where
    title = CustomLicenseTitle ficusMatchDataName
    licenseUnitMatchData =
      LicenseUnitMatchData
        { licenseUnitMatchDataMatchString = Just ficusMatchDataMatchString
        , licenseUnitMatchDataLocation = ficusMatchDataStartByte
        , licenseUnitMatchDataLength = ficusMatchDataEndByte - ficusMatchDataStartByte
        , licenseUnitMatchDataIndex = 1
        , licenseUnitDataStartLine = ficusMatchDataStartLine
        , licenseUnitDataEndLine = ficusMatchDataEndLine
        }

createLicenseUnitDataSingles :: HashMap CustomLicensePath (Maybe Text) -> ((CustomLicensePath, CustomLicenseTitle), LicenseUnitMatchData) -> ((CustomLicensePath, CustomLicenseTitle), LicenseUnitData)
createLicenseUnitDataSingles contents ((path, title), licenseUnitMatchData) =
  ((path, title), newLicenseUnitData)
  where
    newLicenseUnitData =
      LicenseUnitData
        { licenseUnitDataPath = unCustomLicensePath path
        , licenseUnitDataCopyright = Nothing
        , licenseUnitDataThemisVersion = ""
        , licenseUnitDataMatchData = Just $ NE.singleton licenseUnitMatchData
        , licenseUnitDataCopyrights = Nothing
        , licenseUnitDataContents = join $ HashMap.lookup path contents
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
        , licenseUnitNoticeFiles = []
        , licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Just $ "custom license search " <> unCustomLicenseTitle title}
        }
