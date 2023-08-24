{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.LernieSpec (
  spec,
) where

import App.Fossa.Config.Analyze (GrepEntry (..), GrepOptions (..))
import App.Fossa.Lernie.Analyze (analyzeWithLernie, grepOptionsToLernieConfig, lernieMessagesToLernieResults, singletonLernieMessage)
import App.Fossa.Lernie.Types (LernieConfig (..), LernieError (..), LernieMatch (..), LernieMatchData (..), LernieMessage (..), LernieMessages (..), LernieRegex (..), LernieResults (..), LernieScanType (..), LernieWarning (..))
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.String.Conversion (ToText (toText))
import Data.Text qualified as Text
import Effect.Exec (runExecIO)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, Dir, Path, Rel, mkAbsDir, mkRelDir, toFilePath, (</>))
import Path.IO (getCurrentDir)
import ResultUtil (assertOnSuccess)
import Srclib.Types (LicenseScanType (CliLicenseScanned), LicenseSourceUnit (..), LicenseUnit (..), LicenseUnitData (..), LicenseUnitInfo (..), LicenseUnitMatchData (..))
import System.FilePath (pathSeparator)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

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

secondCustomLicenseLernieMatchData :: LernieMatchData
secondCustomLicenseLernieMatchData =
  customLicenseLernieMatchData
    { lernieMatchDataStartByte = 100
    , lernieMatchDataEndByte = 119
    , lernieMatchDataStartLine = 3
    , lernieMatchDataEndLine = 3
    }

customLicenseMatchMessage :: LernieMatch
customLicenseMatchMessage =
  LernieMatch
    { lernieMatchPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , lernieMatchMatches = [customLicenseLernieMatchData]
    }

secondCustomLicenseMatchMessage :: LernieMatch
secondCustomLicenseMatchMessage =
  LernieMatch
    { lernieMatchPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , lernieMatchMatches = [secondCustomLicenseLernieMatchData]
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
    { lernieResultsWarnings = Just $ NE.singleton warningMessage
    , lernieResultsErrors = Just $ NE.singleton errorMessage
    , lernieResultsKeywordSearches = Just $ NE.singleton keywordSearchMatchMessage
    , lernieResultsCustomLicenses = Just $ NE.singleton customLicenseMatchMessage
    , lernieResultsSourceUnit = Just expectedSourceUnit
    }

expectedDoubleLernieResults :: LernieResults
expectedDoubleLernieResults =
  expectedLernieResults
    { lernieResultsCustomLicenses = Just $ NE.fromList [secondCustomLicenseMatchMessage, customLicenseMatchMessage]
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
    , licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}
    }

expectedDoubleSourceUnit :: LicenseSourceUnit
expectedDoubleSourceUnit =
  expectedSourceUnit
    { licenseSourceUnitLicenseUnits = NE.singleton expectedDoubleLicenseUnit
    }

expectedDoubleLicenseUnit :: LicenseUnit
expectedDoubleLicenseUnit =
  expectedLicenseUnit{licenseUnitData = NE.fromList [expectedSecondUnitData, expectedUnitData]}

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

expectedSecondUnitData :: LicenseUnitData
expectedSecondUnitData =
  expectedUnitData{licenseUnitDataMatchData = Just $ NE.singleton expectedSecondLicenseUnitMatchData}

expectedSecondLicenseUnitMatchData :: LicenseUnitMatchData
expectedSecondLicenseUnitMatchData =
  expectedLicenseUnitMatchData
    { licenseUnitMatchDataLocation = 100
    , licenseUnitDataStartLine = 3
    , licenseUnitDataEndLine = 3
    }

grepOptions :: GrepOptions
grepOptions =
  GrepOptions
    { customLicenseSearch = Just $ NE.singleton customLicenseGrepEntry
    , keywordSearch = Just $ NE.singleton keywordSearchGrepEntry
    }

customLicenseGrepEntry :: GrepEntry
customLicenseGrepEntry =
  GrepEntry
    { grepEntryMatchCriteria = "[Pp]roprietary [Ll]icense"
    , grepEntryName = "Proprietary License"
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
    , regexes = NE.fromList [customLicenseLernieRegex, keywordSearchLernieRegex]
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
      (grepOptionsToLernieConfig absDir grepOptions) `shouldBe` Just expectedLernieConfig

  describe "analyzeWithLernie" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir

    let somethingPath = toText . toFilePath $ scanDir </> $(mkRelDir "something.txt")
    let onePath = toText . toFilePath $ scanDir </> $(mkRelDir "one.txt")

    let fixedSomethingPath = fromMaybe somethingPath (Text.stripSuffix (toText pathSeparator) somethingPath)
    let fixedOnePath = fromMaybe onePath (Text.stripSuffix (toText pathSeparator) onePath)

    result <- runIO . runStack . runDiagnostics . runExecIO . runReadFSIO $ analyzeWithLernie scanDir Nothing grepOptions

    it "should analyze a directory" $ do
      -- Fix the paths in the expected data. We need to do this here because they include the full path to the file
      let actualUnitData =
            expectedUnitData
              { licenseUnitDataPath = fixedOnePath
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

      assertOnSuccess result $ \_ maybeRes ->
        case maybeRes of
          Nothing -> expectationFailure "analyzeWithLernie should not return Nothing"
          Just res -> do
            (lernieResultsWarnings res) `shouldBe` Nothing
            (lernieResultsErrors res) `shouldBe` Nothing
            (lernieResultsKeywordSearches res) `shouldBe` Just (NE.singleton keywordSearchMatchMessage{lernieMatchPath = fixedSomethingPath})
            (lernieResultsCustomLicenses res) `shouldBe` Just (NE.singleton customLicenseMatchMessage{lernieMatchPath = fixedOnePath})
            (lernieResultsSourceUnit res) `shouldBe` Just actualSourceUnit
