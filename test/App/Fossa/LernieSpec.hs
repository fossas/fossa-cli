{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.LernieSpec (
  spec,
) where

import App.Fossa.Config.Analyze (GrepEntry (..), GrepOptions (..))
import App.Fossa.Lernie.Analyze (addLernieMessage, analyzeWithLernie, grepOptionsToLernieConfig, lernieMessagesToLernieResults)
import App.Fossa.Lernie.Types (LernieConfig (..), LernieError (..), LernieMatch (..), LernieMatchData (..), LernieMessage (..), LernieMessages (..), LernieRegex (..), LernieResults (..), LernieScanType (..), LernieWarning (..), emptyLernieMessages)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Lift (sendIO)
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

customLicenseMatchMessage :: LernieMatch
customLicenseMatchMessage =
  LernieMatch
    { lernieMatchPath = toText . toFilePath $ absDir </> $(mkRelDir "two.txt")
    , lernieMatchMatches = [customLicenseLernieMatchData]
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
    -- We want to strip the trailing "/" off of the paths, but if there is no
    -- trailing "/", then just return the path
    let somethingPath = toText . toFilePath $ scanDir </> $(mkRelDir "something.txt")
    let fixedSomethingPath = fromMaybe somethingPath (Text.stripSuffix "/" somethingPath)
    let onePath = toText . toFilePath $ scanDir </> $(mkRelDir "one.txt")
    let fixedOnePath = fromMaybe onePath (Text.stripSuffix "/" onePath)
    result <- runIO . runStack . runDiagnostics . runExecIO . runReadFSIO $ analyzeWithLernie scanDir Nothing grepOptions

    it "should analyze a directory" $ do
      sendIO . print $ "scanDir: " ++ show scanDir
      -- Fix the paths in the expected data
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
            (lernieResultsCustomLicenses res) `shouldBe` Just (NE.singleton customLicenseMatchMessage{lernieMatchPath = onePath})
            (lernieResultsSourceUnit res) `shouldBe` Just actualSourceUnit
