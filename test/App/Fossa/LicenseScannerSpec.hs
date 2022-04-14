module App.Fossa.LicenseScannerSpec (spec) where

import App.Fossa.LicenseScanner (combineLicenseUnits)
import Data.List.NonEmpty qualified as NE
import Srclib.Types (
  LicenseUnit (..),
  LicenseUnitData (..),
  LicenseUnitInfo (..),
 )
import Test.Hspec (Spec, describe, it, shouldBe)

info :: LicenseUnitInfo
info = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}

standardData :: LicenseUnitData
standardData =
  LicenseUnitData
    { licenseUnitDataPath = ""
    , licenseUnitDataCopyright = Just ""
    , licenseUnitDataThemisVersion = "123"
    , licenseUnitDataCopyrights = Nothing
    , licenseUnitDataMatchData = Nothing
    }

unitOne :: LicenseUnit
unitOne =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData =
        NE.fromList
          [ standardData{licenseUnitDataPath = "foo/bar/LICENSE"}
          , standardData{licenseUnitDataPath = "foo/bar/one.txt"}
          ]
    , licenseUnitFiles =
        NE.fromList
          [ "foo/bar/LICENSE"
          , "foo/bar/one.txt"
          ]
    , licenseUnitInfo = info
    }

unitTwo :: LicenseUnit
unitTwo =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData =
        NE.fromList
          [ standardData{licenseUnitDataPath = "foo/bar/baz/ANOTHER_LICENSE"}
          , standardData{licenseUnitDataPath = "foo/bar/baz/two.txt"}
          ]
    , licenseUnitFiles =
        NE.fromList
          [ "foo/bar/baz/ANOTHER_LICENSE"
          , "foo/bar/baz/two.txt"
          ]
    , licenseUnitInfo = info
    }
expectedCombinedUnit :: LicenseUnit
expectedCombinedUnit =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData =
        NE.fromList
          [ standardData{licenseUnitDataPath = "foo/bar/LICENSE"}
          , standardData{licenseUnitDataPath = "foo/bar/baz/ANOTHER_LICENSE"}
          , standardData{licenseUnitDataPath = "foo/bar/baz/two.txt"}
          , standardData{licenseUnitDataPath = "foo/bar/one.txt"}
          ]
    , licenseUnitFiles =
        NE.fromList
          [ "foo/bar/LICENSE"
          , "foo/bar/baz/ANOTHER_LICENSE"
          , "foo/bar/baz/two.txt"
          , "foo/bar/one.txt"
          ]
    , licenseUnitInfo = info
    }

spec :: Spec
spec =
  -- this test only exists to prevent merging the commented out analyzers
  describe "combineLicenseUnits" $ do
    it "should combine two MIT units" $
      combineLicenseUnits [unitOne, unitTwo] `shouldBe` [expectedCombinedUnit]
    it "should not combine two units with different licenses" $
      combineLicenseUnits [unitOne, unitTwo{licenseUnitName = "AGPL"}] `shouldBe` [unitTwo{licenseUnitName = "AGPL"}, unitOne]
