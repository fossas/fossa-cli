{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.LernieSpec (
  spec,
) where

import App.Fossa.Lernie.Analyze (analyzeWithLernie, analyzeWithLernieWithOrgInfo, grepOptionsToLernieConfig, lernieMessagesToLernieResults, singletonLernieMessage)
import App.Fossa.Lernie.Types (GrepEntry (..), GrepOptions (..), LernieConfig (..), LernieError (..), LernieMatch (..), LernieMatchData (..), LernieMessage (..), LernieMessages (..), LernieRegex (..), LernieResults (..), LernieScanType (..), LernieWarning (..), OrgWideCustomLicenseConfigPolicy (..))
import App.Types (FileUpload (..))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Telemetry (withoutTelemetry)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
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

customLicenseLernieMatchData :: LernieMatchData
customLicenseLernieMatchData =
  LernieMatchData
    { lernieMatchDataPattern = "[Pp]roprietary [Ll]icense"
    , lernieMatchDataMatchString = "Proprietary License"
    , lernieMatchDataScanType = CustomLicense
    , lernieMatchDataName = "Proprietary License"
    , lernieMatchDataStartByte = 10
    , lernieMatchDataEndByte = 29
    , lernieMatchDataStartLine = 1
    , lernieMatchDataEndLine = 1
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

secondCustomLicenseLernieMatchData :: LernieMatchData
secondCustomLicenseLernieMatchData =
  customLicenseLernieMatchData
    { lernieMatchDataStartByte = 42 + extraLineBytes * 2
    , lernieMatchDataEndByte = 61 + extraLineBytes * 2
    , lernieMatchDataStartLine = 3
    , lernieMatchDataEndLine = 3
    }

thirdCustomLicenseLernieMatchData :: LernieMatchData
thirdCustomLicenseLernieMatchData =
  customLicenseLernieMatchData
    { lernieMatchDataStartByte = 85 + extraLineBytes * 4
    , lernieMatchDataEndByte = 104 + extraLineBytes * 4
    , lernieMatchDataStartLine = 5
    , lernieMatchDataEndLine = 5
    }

customLicenseMatchMessage :: LernieMatch
customLicenseMatchMessage =
  LernieMatch
    { lernieMatchPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , lernieMatchMatches = [customLicenseLernieMatchData]
    , lernieMatchContents = Nothing
    }

secondCustomLicenseMatchMessage :: LernieMatch
secondCustomLicenseMatchMessage =
  LernieMatch
    { lernieMatchPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , lernieMatchMatches = [secondCustomLicenseLernieMatchData]
    , lernieMatchContents = Nothing
    }

keywordSearchLernieMatchData :: LernieMatchData
keywordSearchLernieMatchData =
  LernieMatchData
    { lernieMatchDataPattern = "[Kk]eyword [Ss]earch"
    , lernieMatchDataMatchString = "Keyword Search"
    , lernieMatchDataScanType = KeywordSearch
    , lernieMatchDataName = "Keyword Search"
    , lernieMatchDataStartByte = 0
    , lernieMatchDataEndByte = 14
    , lernieMatchDataStartLine = 1
    , lernieMatchDataEndLine = 1
    }

keywordSearchMatchMessage :: LernieMatch
keywordSearchMatchMessage =
  LernieMatch
    { lernieMatchPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , lernieMatchMatches = [keywordSearchLernieMatchData]
    , lernieMatchContents = Nothing
    }

warningMessage :: LernieWarning
warningMessage =
  LernieWarning
    { lernieWarningMessage = "this is a warning"
    , lernieWarningType = "SomeWarningType"
    }

errorMessage :: LernieError
errorMessage =
  LernieError
    { lernieErrorMessage = "this is an Error"
    , lernieErrorType = "SomeWarningType"
    }

filledInMessages :: LernieMessages
filledInMessages =
  singletonLernieMessage (LernieMessageLernieError errorMessage)
    <> singletonLernieMessage (LernieMessageLernieWarning warningMessage)
    <> singletonLernieMessage (LernieMessageLernieMatch keywordSearchMatchMessage)
    <> singletonLernieMessage (LernieMessageLernieMatch customLicenseMatchMessage)

doubleMessages :: LernieMessages
doubleMessages = singletonLernieMessage (LernieMessageLernieMatch secondCustomLicenseMatchMessage) <> filledInMessages

expectedLernieResults :: LernieResults
expectedLernieResults =
  LernieResults
    { lernieResultsKeywordSearches = [keywordSearchMatchMessage]
    , lernieResultsCustomLicenses = [customLicenseMatchMessage]
    , lernieResultsSourceUnit = Just expectedSourceUnit
    }

expectedDoubleLernieResults :: LernieResults
expectedDoubleLernieResults =
  expectedLernieResults
    { lernieResultsCustomLicenses = [secondCustomLicenseMatchMessage, customLicenseMatchMessage]
    , lernieResultsSourceUnit = Just expectedDoubleSourceUnit
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

expectedLernieConfig :: LernieConfig
expectedLernieConfig =
  LernieConfig
    { rootDir = absDir
    , regexes = [customLicenseLernieRegex, keywordSearchLernieRegex]
    , fullFiles = False
    }

keywordSearchLernieRegex :: LernieRegex
keywordSearchLernieRegex =
  LernieRegex
    { pat = "[Kk]eyword [Ss]earch"
    , name = "Keyword Search"
    , scanType = KeywordSearch
    }

customLicenseLernieRegex :: LernieRegex
customLicenseLernieRegex =
  LernieRegex
    { pat = "[Pp]roprietary [Ll]icense"
    , name = "Proprietary License"
    , scanType = CustomLicense
    }

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/Lernie/testdata/repo")

spec :: Spec
spec = do
  describe "lernieMessagesToLernieResults" $ do
    it "should create a proper LernieResults" $ do
      lernieMessagesToLernieResults filledInMessages absDir `shouldBe` expectedLernieResults

    it "should deal properly with two of the same license found in one file" $ do
      lernieMessagesToLernieResults doubleMessages absDir `shouldBe` expectedDoubleLernieResults

  describe "addLernieMessage" $ do
    it "should add a match to matches" $ do
      (lernieMessageMatches filledInMessages) `shouldBe` [keywordSearchMatchMessage, customLicenseMatchMessage]

    it "should add a warning to warnings" $ do
      (lernieMessageWarnings filledInMessages) `shouldBe` [warningMessage]

    it "should add an error to errors" $ do
      (lernieMessageErrors filledInMessages) `shouldBe` [errorMessage]

  describe "grepOptionsToLernieConfig" $ do
    it "should create a lernie config" $ do
      grepOptionsToLernieConfig absDir grepOptions FileUploadMatchData `shouldBe` Just expectedLernieConfig

  describe "analyzeWithLernie" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir

    let somethingPath = toText . toFilePath $ scanDir </> $(mkRelDir "something.txt")
    let onePath = toText . toFilePath $ scanDir </> $(mkRelDir "one.txt")

    let fixedSomethingPath = fromMaybe somethingPath (Text.stripSuffix (toText pathSeparator) somethingPath)
    let fixedOnePath = fromMaybe onePath (Text.stripSuffix (toText pathSeparator) onePath)

    it' "should analyze a directory with the provided config if no API keys are passed in" $ do
      result <- ignoreDebug . withoutTelemetry $ analyzeWithLernie scanDir Nothing grepOptions{configFilePath = (Just $ scanDir </> $(mkRelFile ".fossa.yml"))}
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
        Nothing -> expectationFailure' "analyzeWithLernie should not return Nothing"
        Just res -> do
          (lernieResultsKeywordSearches res) `shouldBe'` [keywordSearchMatchMessage{lernieMatchPath = fixedSomethingPath}]
          (lernieResultsCustomLicenses res)
            `shouldBe'` [ customLicenseMatchMessage
                            { lernieMatchPath = fixedOnePath
                            , lernieMatchMatches = [customLicenseLernieMatchData, secondCustomLicenseLernieMatchData, thirdCustomLicenseLernieMatchData]
                            }
                        ]
          (lernieResultsSourceUnit res) `shouldBe'` Just actualSourceUnit

    it' "should include the file contents if the org has the full-files flag on" $ do
      GetOrganization `alwaysReturns` Fixtures.organization{orgCustomLicenseScanConfigs = [secondCustomLicenseGrepEntry], orgRequiresFullFileUploads = True}
      result <- ignoreDebug . withoutTelemetry $ analyzeWithLernieWithOrgInfo scanDir grepOptions
      case result of
        Nothing -> expectationFailure' "analyzeWithLernie should not return Nothing"
        Just res -> do
          -- Just assert that we find the contents of the files
          let sourceUnit = lernieResultsSourceUnit res
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
      result <- ignoreDebug . withoutTelemetry $ analyzeWithLernieWithOrgInfo scanDir grepOptions
      case result of
        Nothing -> expectationFailure' "analyzeWithLernie should not return Nothing"
        Just res -> do
          -- Just assert that the contents are all `Nothing`
          let sourceUnit = lernieResultsSourceUnit res
          let maybeLicenseUnits = licenseSourceUnitLicenseUnits <$> sourceUnit
          case maybeLicenseUnits of
            Nothing -> expectationFailure' "licenseUnits should not be Nothing"
            Just licenseUnits -> do
              let licenseUnitDatas = concatMap (NE.toList . licenseUnitData) licenseUnits
              let contents = map licenseUnitDataContents licenseUnitDatas
              contents `shouldBe'` [Nothing, Nothing, Nothing]

    it' "should merge the config from fossa.yml and the org" $ do
      GetOrganization `alwaysReturns` Fixtures.organization{orgCustomLicenseScanConfigs = [secondCustomLicenseGrepEntry]}
      result <- ignoreDebug . withoutTelemetry $ analyzeWithLernieWithOrgInfo scanDir grepOptions
      case result of
        Nothing -> expectationFailure' "analyzeWithLernie should not return Nothing"
        Just res -> do
          -- Just assert that we find matches for "Confidential" (from the org API) and "Proprietary License" (from grepOptions)
          let matchNames = lernieMatchDataName <$> concatMap lernieMatchMatches (lernieResultsCustomLicenses res)
          sort (nub matchNames) `shouldBe'` ["Confidential", "Proprietary License"]
