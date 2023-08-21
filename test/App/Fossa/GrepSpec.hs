{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.GrepSpec (
  spec,
) where

import App.Fossa.Config.Analyze (GrepEntry (..), GrepOptions (..))
import App.Fossa.Grep (GrepScanType (..), LernieConfig (..), LernieError (..), LernieMatch (..), LernieMatchData (..), LernieMessage (..), LernieMessages (..), LernieRegex (..), LernieResults (..), LernieWarning (..), addLernieMessage, emptyLernieMessages, grepOptionsToLernieConfig, lernieMessagesToLernieResults)
import Data.List.NonEmpty qualified as NE
import Path (Abs, Dir, Path, mkAbsDir)
import Srclib.Types (LicenseScanType (CliLicenseScanned), LicenseSourceUnit (..), LicenseUnit (..), LicenseUnitData (..), LicenseUnitInfo (..), LicenseUnitMatchData (..))
import Test.Hspec (Spec, describe, it, shouldBe)

customLicenseLernieMatchData :: LernieMatchData
customLicenseLernieMatchData =
  LernieMatchData
    { lernieMatchDataPattern = "[Pp]roprietary"
    , lernieMatchDataMatchString = "proprietary"
    , lernieMatchDataScanType = CustomLicense
    , lernieMatchDataName = "Proprietary License"
    , lernieMatchDataStartByte = 123
    , lernieMatchDataEndByte = 195
    , lernieMatchDataStartLine = 3
    , lernieMatchDataEndLine = 3
    }

customLicenseMatchMessage :: LernieMatch
customLicenseMatchMessage =
  LernieMatch
    { lernieMatchPath = "/tmp/one/two"
    , lernieMatchMatches = [customLicenseLernieMatchData]
    }

keywordSearchLernieMatchData :: LernieMatchData
keywordSearchLernieMatchData =
  LernieMatchData
    { lernieMatchDataPattern = "[Kk]eyword"
    , lernieMatchDataMatchString = "keyword"
    , lernieMatchDataScanType = KeywordSearch
    , lernieMatchDataName = "Keyword Search"
    , lernieMatchDataStartByte = 111
    , lernieMatchDataEndByte = 122
    , lernieMatchDataStartLine = 4
    , lernieMatchDataEndLine = 4
    }

keywordSearchMatchMessage :: LernieMatch
keywordSearchMatchMessage =
  LernieMatch
    { lernieMatchPath = "/tmp/one/two"
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
    { lernieErrorMessage = "this is a Error"
    , lernieErrorType = "SomeWarningType"
    }

filledInMessages :: LernieMessages
filledInMessages =
  addLernieMessage (LernieMessageLernieError errorMessage) $
    addLernieMessage (LernieMessageLernieWarning warningMessage) $
      addLernieMessage (LernieMessageLernieMatch keywordSearchMatchMessage) $
        addLernieMessage (LernieMessageLernieMatch customLicenseMatchMessage) emptyLernieMessages

expectedLernieResults :: LernieResults
expectedLernieResults =
  LernieResults
    { lernieResultsWarnings = Just $ NE.singleton warningMessage
    , lernieResultsErrors = Just $ NE.singleton errorMessage
    , lernieResultsKeywordSearches = Just $ NE.singleton keywordSearchMatchMessage
    , lernieResultsCustomLicenses = Just $ NE.singleton customLicenseMatchMessage
    , lernieResultsSourceUnit = Just expectedSourceUnit
    }

expectedSourceUnit :: LicenseSourceUnit
expectedSourceUnit =
  LicenseSourceUnit
    { licenseSourceUnitName = "/tmp/one/"
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
    , licenseUnitFiles = NE.singleton "/tmp/one/two"
    , licenseUnitData = NE.singleton expectedUnitData
    , licenseUnitInfo = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}
    }

expectedUnitData :: LicenseUnitData
expectedUnitData =
  LicenseUnitData
    { licenseUnitDataPath = "/tmp/one/two"
    , licenseUnitDataCopyright = Nothing
    , licenseUnitDataThemisVersion = ""
    , licenseUnitDataMatchData = Just $ NE.singleton expectedLicenseUnitMatchData
    , licenseUnitDataCopyrights = Nothing
    , licenseUnitDataContents = Nothing
    }

expectedLicenseUnitMatchData :: LicenseUnitMatchData
expectedLicenseUnitMatchData =
  LicenseUnitMatchData
    { licenseUnitMatchDataMatchString = Just "proprietary"
    , licenseUnitMatchDataLocation = 123
    , licenseUnitMatchDataLength = 72
    , licenseUnitMatchDataIndex = 1
    , licenseUnitDataStartLine = 3
    , licenseUnitDataEndLine = 3
    }

#ifdef mingw32_HOST_OS
absDir :: Path Abs Dir
absDir = $(mkAbsDir "C:/")
#else
absDir :: Path Abs Dir
absDir = $(mkAbsDir "/tmp/one")
#endif

grepOptions :: GrepOptions
grepOptions =
  GrepOptions
    { customLicenseSearch = Just $ NE.singleton customLicenseGrepEntry
    , keywordSearch = Just $ NE.singleton keywordSearchGrepEntry
    }

customLicenseGrepEntry :: GrepEntry
customLicenseGrepEntry =
  GrepEntry
    { grepEntryMatchCriteria = "customLicensePattern"
    , grepEntryName = "some custom license"
    }

keywordSearchGrepEntry :: GrepEntry
keywordSearchGrepEntry =
  GrepEntry
    { grepEntryMatchCriteria = "keywordSearchPattern"
    , grepEntryName = "some keyword search"
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
    { pat = "keywordSearchPattern"
    , name = "some keyword search"
    , scanType = KeywordSearch
    }

customLicenseLernieRegex :: LernieRegex
customLicenseLernieRegex =
  LernieRegex
    { pat = "customLicensePattern"
    , name = "some custom license"
    , scanType = CustomLicense
    }

spec :: Spec
spec = do
  describe "lernieMessagesToLernieResults" $ do
    it "should create a proper LernieResults" $ do
      lernieMessagesToLernieResults filledInMessages absDir `shouldBe` expectedLernieResults

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
