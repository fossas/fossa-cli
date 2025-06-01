{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.FicusSpec (
  spec,
) where

import App.Fossa.Ficus.Analyze (analyzeWithFicus, analyzeWithFicusWithOrgInfo, ficusMessagesToFicusResults, grepOptionsToFicusConfig, singletonFicusMessage)
import App.Fossa.Ficus.Types (FicusConfig (..), FicusError (..), FicusMatch (..), FicusMatchData (..), FicusMessage (..), FicusMessages (..), FicusRegex (..), FicusResults (..), FicusScanType (..), FicusWarning (..), GrepEntry (..), GrepOptions (..), OrgWideCustomLicenseConfigPolicy (..))
import App.Types (FileUpload (..))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Telemetry (withoutTelemetry)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, catMaybes)
import Data.String.Conversion (ToText (toText))
import Data.Text qualified as Text
import Fossa.API.Types (Organization (..))
import Path (Abs, Dir, Path, Rel, mkAbsDir, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Srclib.Types (LicenseScanType (CliLicenseScanned), LicenseSourceUnit (..), LicenseUnit (..), LicenseUnitData (..), LicenseUnitInfo (..), LicenseUnitMatchData (..))
import System.FilePath (pathSeparator)
import Test.Effect (expectationFailure', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.MockApi (alwaysReturns)
import Types (GlobFilter (GlobFilter), LicenseScanPathFilters (..))

customLicenseFicusMatchData :: FicusMatchData
customLicenseFicusMatchData =
  FicusMatchData
    { ficusMatchDataPattern = "[Pp]roprietary [Ll]icense"
    , ficusMatchDataMatchString = "Proprietary License"
    , ficusMatchDataScanType = CustomLicense
    , ficusMatchDataName = "Proprietary License"
    , ficusMatchDataStartByte = 10
    , ficusMatchDataEndByte = 29
    , ficusMatchDataStartLine = 1
    , ficusMatchDataEndLine = 1
    }

-- Every newline in Windows adds one more byte than a newLine on macOS or Linux
-- To calculate byte offsets that work on both Windows and other OSes, add
-- `extraLineBytes * (lineNumber - 1)` to the bytes.
-- (line-numbers are 1-indexed, and you have only encountered lineNumber - 1 newLines when you are on line n,
-- so you have to subtract 1 from them).

extraLineBytes :: Integer
#ifdef mingw32_HOST_OS
extraLineBytes = 1
#else
extraLineBytes = 0
#endif

secondCustomLicenseFicusMatchData :: FicusMatchData
secondCustomLicenseFicusMatchData =
  customLicenseFicusMatchData
    { ficusMatchDataStartByte = 42 + extraLineBytes * 2
    , ficusMatchDataEndByte = 61 + extraLineBytes * 2
    , ficusMatchDataStartLine = 3
    , ficusMatchDataEndLine = 3
    }

thirdCustomLicenseFicusMatchData :: FicusMatchData
thirdCustomLicenseFicusMatchData =
  customLicenseFicusMatchData
    { ficusMatchDataStartByte = 85 + extraLineBytes * 4
    , ficusMatchDataEndByte = 104 + extraLineBytes * 4
    , ficusMatchDataStartLine = 5
    , ficusMatchDataEndLine = 5
    }

customLicenseMatchMessage :: FicusMatch
customLicenseMatchMessage =
  FicusMatch
    { ficusMatchPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , ficusMatchMatches = [customLicenseFicusMatchData]
    , ficusMatchContents = Nothing
    }

secondCustomLicenseMatchMessage :: FicusMatch
secondCustomLicenseMatchMessage =
  FicusMatch
    { ficusMatchPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , ficusMatchMatches = [secondCustomLicenseFicusMatchData]
    , ficusMatchContents = Nothing
    }

keywordSearchFicusMatchData :: FicusMatchData
keywordSearchFicusMatchData =
  FicusMatchData
    { ficusMatchDataPattern = "[Kk]eyword [Ss]earch"
    , ficusMatchDataMatchString = "Keyword Search"
    , ficusMatchDataScanType = KeywordSearch
    , ficusMatchDataName = "Keyword Search"
    , ficusMatchDataStartByte = 0
    , ficusMatchDataEndByte = 14
    , ficusMatchDataStartLine = 1
    , ficusMatchDataEndLine = 1
    }

keywordSearchMatchMessage :: FicusMatch
keywordSearchMatchMessage =
  FicusMatch
    { ficusMatchPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , ficusMatchMatches = [keywordSearchFicusMatchData]
    , ficusMatchContents = Nothing
    }

warningMessage :: FicusWarning
warningMessage =
  FicusWarning
    { ficusWarningMessage = "this is a warning"
    , ficusWarningType = "SomeWarningType"
    }

errorMessage :: FicusError
errorMessage =
  FicusError
    { ficusErrorMessage = "this is an Error"
    , ficusErrorType = "SomeWarningType"
    }

filledInMessages :: FicusMessages
filledInMessages =
  singletonFicusMessage (FicusMessageFicusError errorMessage)
    <> singletonFicusMessage (FicusMessageFicusWarning warningMessage)
    <> singletonFicusMessage (FicusMessageFicusMatch keywordSearchMatchMessage)
    <> singletonFicusMessage (FicusMessageFicusMatch customLicenseMatchMessage)

doubleMessages :: FicusMessages
doubleMessages = singletonFicusMessage (FicusMessageFicusMatch secondCustomLicenseMatchMessage) <> filledInMessages

expectedFicusResults :: FicusResults
expectedFicusResults =
  FicusResults
    { ficusResultsKeywordSearches = [keywordSearchMatchMessage]
    , ficusResultsCustomLicenses = [customLicenseMatchMessage]
    , ficusResultsSourceUnit = Just expectedSourceUnit
    }

expectedDoubleFicusResults :: FicusResults
expectedDoubleFicusResults =
  expectedFicusResults
    { ficusResultsCustomLicenses = [secondCustomLicenseMatchMessage, customLicenseMatchMessage]
    , ficusResultsSourceUnit = Just expectedDoubleSourceUnit
    }

absDir :: Path Abs Dir
#ifdef mingw32_HOST_OS
absDir = $(mkAbsDir "C:/")
#else
absDir = $(mkAbsDir "/tmp/one")
#endif

expectedSourceUnit :: LicenseSourceUnit
expectedSourceUnit =
  LicenseSourceUnit
    { licenseSourceUnitName = toText . toFilePath $ absDir
    , licenseSourceUnitType = CliLicenseScanned
    , licenseSourceUnitLicenseUnits = NE.singleton expectedLicenseUnit
    }

expectedLicenseUnit :: LicenseUnit
expectedLicenseUnit =
  LicenseUnit
    { licenseUnitName = "custom-license"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitTitle = Just "Proprietary License"
    , licenseUnitDir = ""
    , licenseUnitFiles = NE.singleton $ toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , licenseUnitData = NE.singleton expectedUnitData
    , licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Just "custom license search Proprietary License"}
    , licenseUnitNoticeFiles = []
    }

expectedDoubleSourceUnit :: LicenseSourceUnit
expectedDoubleSourceUnit =
  expectedSourceUnit
    { licenseSourceUnitLicenseUnits = NE.singleton expectedDoubleLicenseUnit
    }

expectedDoubleLicenseUnit :: LicenseUnit
expectedDoubleLicenseUnit =
  expectedLicenseUnit{licenseUnitData = NE.singleton expectedDoubleUnitData}

expectedUnitData :: LicenseUnitData
expectedUnitData =
  LicenseUnitData
    { licenseUnitDataPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , licenseUnitDataCopyright = Nothing
    , licenseUnitDataThemisVersion = ""
    , licenseUnitDataMatchData = Just $ NE.singleton expectedLicenseUnitMatchData
    , licenseUnitDataCopyrights = Nothing
    , licenseUnitDataContents = Nothing
    }

expectedDoubleUnitData :: LicenseUnitData
expectedDoubleUnitData =
  expectedUnitData
    { licenseUnitDataMatchData = Just $ NE.fromList [expectedLicenseUnitMatchData, expectedSecondLicenseUnitMatchData]
    }

expectedLicenseUnitMatchData :: LicenseUnitMatchData
expectedLicenseUnitMatchData =
  LicenseUnitMatchData
    { licenseUnitMatchDataMatchString = Just "Proprietary License"
    , licenseUnitMatchDataLocation = 10
    , licenseUnitMatchDataLength = 19
    , licenseUnitMatchDataIndex = 1
    , licenseUnitDataStartLine = 1
    , licenseUnitDataEndLine = 1
    }

expectedSecondLicenseUnitMatchData :: LicenseUnitMatchData
expectedSecondLicenseUnitMatchData =
  expectedLicenseUnitMatchData
    { licenseUnitMatchDataLocation = 42 + extraLineBytes * 2
    , licenseUnitDataStartLine = 3
    , licenseUnitDataEndLine = 3
    }

expectedThirdLicenseUnitMatchData :: LicenseUnitMatchData
expectedThirdLicenseUnitMatchData =
  expectedLicenseUnitMatchData
    { licenseUnitMatchDataLocation = 85 + extraLineBytes * 4
    , licenseUnitDataStartLine = 5
    , licenseUnitDataEndLine = 5
    }

grepOptions :: GrepOptions
grepOptions =
  GrepOptions
    { customLicenseSearch = [customLicenseGrepEntry]
    , keywordSearch = [keywordSearchGrepEntry]
    , orgWideCustomLicenseScanConfigPolicy = Use
    , configFilePath = Nothing
    }

customLicenseGrepEntry :: GrepEntry
customLicenseGrepEntry =
  GrepEntry
    { grepEntryMatchCriteria = "[Pp]roprietary [Ll]icense"
    , grepEntryName = "Proprietary License"
    }

secondCustomLicenseGrepEntry :: GrepEntry
secondCustomLicenseGrepEntry =
  GrepEntry
    { grepEntryMatchCriteria = "[Cc]onfidential"
    , grepEntryName = "Confidential"
    }

keywordSearchGrepEntry :: GrepEntry
keywordSearchGrepEntry =
  GrepEntry
    { grepEntryMatchCriteria = "[Kk]eyword [Ss]earch"
    , grepEntryName = "Keyword Search"
    }

expectedLicenseScanPathFilters :: Maybe LicenseScanPathFilters
expectedLicenseScanPathFilters =
  Just
    LicenseScanPathFilters
      { licenseScanPathFiltersOnly = [GlobFilter "/*", GlobFilter "/**"]
      , licenseScanPathFiltersExclude = []
      , licenseScanPathFilterFileExclude = []
      }

expectedFicusConfig :: FicusConfig
expectedFicusConfig =
  FicusConfig
    { rootDir = absDir
    , regexes = [customLicenseFicusRegex, keywordSearchFicusRegex]
    , licenseScanPathFilters = expectedLicenseScanPathFilters
    , fullFiles = False
    }

keywordSearchFicusRegex :: FicusRegex
keywordSearchFicusRegex =
  FicusRegex
    { pat = "[Kk]eyword [Ss]earch"
    , name = "Keyword Search"
    , scanType = KeywordSearch
    }

customLicenseFicusRegex :: FicusRegex
customLicenseFicusRegex =
  FicusRegex
    { pat = "[Pp]roprietary [Ll]icense"
    , name = "Proprietary License"
    , scanType = CustomLicense
    }

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/Ficus/testdata/repo")

spec :: Spec
spec = do
  describe "ficusMessagesToFicusResults" $ do
    it "should create a proper FicusResults" $ do
      ficusMessagesToFicusResults filledInMessages absDir `shouldBe` expectedFicusResults

    it "should deal properly with two of the same license found in one file" $ do
      ficusMessagesToFicusResults doubleMessages absDir `shouldBe` expectedDoubleFicusResults

  describe "addFicusMessage" $ do
    it "should add a match to matches" $ do
      (ficusMessageMatches filledInMessages) `shouldBe` [keywordSearchMatchMessage, customLicenseMatchMessage]

    it "should add a warning to warnings" $ do
      (ficusMessageWarnings filledInMessages) `shouldBe` [warningMessage]

    it "should add an error to errors" $ do
      (ficusMessageErrors filledInMessages) `shouldBe` [errorMessage]

  describe "grepOptionsToFicusConfig" $ do
    it "should create a ficus config" $ do
      grepOptionsToFicusConfig absDir grepOptions expectedLicenseScanPathFilters FileUploadMatchData `shouldBe` Just expectedFicusConfig

  describe "analyzeWithFicus" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir

    let somethingPath = toText . toFilePath $ scanDir </> $(mkRelDir "something.txt")
    let onePath = toText . toFilePath $ scanDir </> $(mkRelDir "one.txt")

    let fixedSomethingPath = fromMaybe somethingPath (Text.stripSuffix (toText pathSeparator) somethingPath)
    let fixedOnePath = fromMaybe onePath (Text.stripSuffix (toText pathSeparator) onePath)

    it' "should analyze a directory with the provided config if no API keys are passed in" $ do
      result <- ignoreDebug . withoutTelemetry $ analyzeWithFicus scanDir Nothing grepOptions{configFilePath = (Just $ scanDir </> $(mkRelFile ".fossa.yml"))} Nothing
      -- Fix the paths in the expected data. We need to do this here because they include the full path to the file
      let actualUnitData =
            expectedUnitData
              { licenseUnitDataPath = fixedOnePath
              , licenseUnitDataMatchData = Just $ NE.fromList [expectedThirdLicenseUnitMatchData, expectedSecondLicenseUnitMatchData, expectedLicenseUnitMatchData]
              }
          actualLicenseUnit =
            expectedLicenseUnit
              { licenseUnitFiles = NE.singleton fixedOnePath
              , licenseUnitData = NE.singleton actualUnitData
              }
          actualSourceUnit =
            LicenseSourceUnit
              { licenseSourceUnitName = toText . toFilePath $ scanDir
              , licenseSourceUnitType = CliLicenseScanned
              , licenseSourceUnitLicenseUnits = NE.singleton actualLicenseUnit
              }
      case result of
        Nothing -> expectationFailure' "analyzeWithFicus should not return Nothing"
        Just res -> do
          (ficusResultsKeywordSearches res) `shouldBe'` [keywordSearchMatchMessage{ficusMatchPath = fixedSomethingPath}]
          (ficusResultsCustomLicenses res)
            `shouldBe'` [ customLicenseMatchMessage
                            { ficusMatchPath = fixedOnePath
                            , ficusMatchMatches = [customLicenseFicusMatchData, secondCustomLicenseFicusMatchData, thirdCustomLicenseFicusMatchData]
                            }
                        ]
          (ficusResultsSourceUnit res) `shouldBe'` Just actualSourceUnit

    it' "should include the file contents if the org has the full-files flag on" $ do
      GetOrganization `alwaysReturns` Fixtures.organization{orgCustomLicenseScanConfigs = [secondCustomLicenseGrepEntry], orgRequiresFullFileUploads = True}
      result <- ignoreDebug . withoutTelemetry $ analyzeWithFicusWithOrgInfo scanDir grepOptions Nothing
      case result of
        Nothing -> expectationFailure' "analyzeWithFicus should not return Nothing"
        Just res -> do
          -- Just assert that we find the contents of the files
          let sourceUnit = ficusResultsSourceUnit res
          let maybeLicenseUnits = licenseSourceUnitLicenseUnits <$> sourceUnit
          case maybeLicenseUnits of
            Nothing -> expectationFailure' "licenseUnits should not be Nothing"
            Just licenseUnits -> do
              let licenseUnitDatas = concatMap (NE.toList . licenseUnitData) licenseUnits
              let contents = map licenseUnitDataContents licenseUnitDatas
              -- fix newlines on windows so that we get the same contents on both Windows and other OSes
              let fixedContents = sort $ map (Text.replace "\r\n" "\n" <$>) contents
              -- The result from the .fossa.yml file will be filtered out in actual usage
              fixedContents
                `shouldBe'` [ Just "# I should not find a Proprietary License in this file, because it is the .fossa.yml file\nversion: 3\n"
                            , Just "Keyword Searches are great!\n\nThis file is very confidential\n"
                            , Just "This is a Proprietary License.\n\nIs this a Proprietary License too?\n\nThrow in a third Proprietary License just for fun\n"
                            ]

    it' "should not include the file contents if the org has the full-files flag off" $ do
      GetOrganization `alwaysReturns` Fixtures.organization{orgCustomLicenseScanConfigs = [secondCustomLicenseGrepEntry], orgRequiresFullFileUploads = False}
      result <- ignoreDebug . withoutTelemetry $ analyzeWithFicusWithOrgInfo scanDir grepOptions Nothing
      case result of
        Nothing -> expectationFailure' "analyzeWithFicus should not return Nothing"
        Just res -> do
          -- Just assert that the contents are all `Nothing`
          let sourceUnit = ficusResultsSourceUnit res
          let maybeLicenseUnits = licenseSourceUnitLicenseUnits <$> sourceUnit
          case maybeLicenseUnits of
            Nothing -> expectationFailure' "licenseUnits should not be Nothing"
            Just licenseUnits -> do
              let licenseUnitDatas = concatMap (NE.toList . licenseUnitData) licenseUnits
              let contents = map licenseUnitDataContents licenseUnitDatas
              contents `shouldBe'` [Nothing, Nothing, Nothing]

    it' "should merge the config from fossa.yml and the org" $ do
      GetOrganization `alwaysReturns` Fixtures.organization{orgCustomLicenseScanConfigs = [secondCustomLicenseGrepEntry]}
      result <- ignoreDebug . withoutTelemetry $ analyzeWithFicusWithOrgInfo scanDir grepOptions Nothing
      case result of
        Nothing -> expectationFailure' "analyzeWithFicus should not return Nothing"
        Just res -> do
          -- Just assert that we find matches for "Confidential" (from the org API) and "Proprietary License" (from grepOptions)
          let matchNames = ficusMatchDataName <$> concatMap ficusMatchMatches (ficusResultsCustomLicenses res)
          sort (nub matchNames) `shouldBe'` ["Confidential", "Proprietary License"]

    it' "should handle Nothing licenseScanPathFilters without crashing" $ do
      result <- ignoreDebug . withoutTelemetry $ analyzeWithFicus scanDir Nothing grepOptions Nothing
      case result of
        Nothing -> expectationFailure' "analyzeWithFicus should not return Nothing"
        Just _ -> pure ()

    it' "should apply licenseScanPathFilters' only filter correctly" $ do
      let filters =
            LicenseScanPathFilters
              { licenseScanPathFiltersOnly = [GlobFilter "**/*one.txt"]
              , licenseScanPathFiltersExclude = [GlobFilter "**/*something.txt"]
              , licenseScanPathFilterFileExclude = []
              }

      result <- ignoreDebug . withoutTelemetry $ analyzeWithFicus scanDir Nothing grepOptions (Just filters)
      case result of
        Nothing -> expectationFailure' "analyzeWithFicus should not return Nothing when given valid licenseScanPathFilters"
        Just res -> do
          let matchPaths = sort $ nub $ map ficusMatchPath (ficusResultsCustomLicenses res ++ ficusResultsKeywordSearches res)
          matchPaths `shouldBe'` [fixedOnePath]

    it' "should apply licenseScanPathFilters' exclude filter correctly" $ do
      let filters =
            LicenseScanPathFilters
              { licenseScanPathFiltersOnly = [GlobFilter "**/*.txt"]
              , licenseScanPathFiltersExclude = [GlobFilter "**/*something.txt"]
              , licenseScanPathFilterFileExclude = []
              }

      result <- ignoreDebug . withoutTelemetry $ analyzeWithFicus scanDir Nothing grepOptions (Just filters)
      case result of
        Nothing -> expectationFailure' "analyzeWithFicus should not return Nothing when given valid licenseScanPathFilters"
        Just res -> do
          let matchPaths = sort $ nub $ map ficusMatchPath (ficusResultsCustomLicenses res ++ ficusResultsKeywordSearches res)
          matchPaths `shouldBe'` [fixedOnePath]
