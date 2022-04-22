module App.Fossa.LicenseScannerSpec (spec) where

import App.Fossa.LicenseScanner (combineLicenseUnits)
import Data.List.NonEmpty qualified as NE
import Srclib.Types (
  LicenseUnit (..),
  LicenseUnitData (..),
  LicenseUnitInfo (..),
  emptyLicenseUnitData,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

info :: LicenseUnitInfo
info = LicenseUnitInfo{licenseUnitInfoDescription = Just ""}

unitOne :: LicenseUnit
unitOne =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData = emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/LICENSE"} NE.:| [emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/one.txt"}]
    , licenseUnitFiles =
        "foo/bar/LICENSE" NE.:| ["foo/bar/one.txt"]
    , licenseUnitInfo = info
    }

unitTwo :: LicenseUnit
unitTwo =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData = emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/ANOTHER_LICENSE"} NE.:| [emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/two.txt"}]
    , licenseUnitFiles = "foo/bar/baz/ANOTHER_LICENSE" NE.:| ["foo/bar/baz/two.txt"]
    , licenseUnitInfo = info
    }
expectedCombinedUnit :: LicenseUnit
expectedCombinedUnit =
  LicenseUnit
    { licenseUnitName = "MIT"
    , licenseUnitType = "LicenseUnit"
    , licenseUnitDir = ""
    , licenseUnitData =
        emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/LICENSE"}
          NE.:| [ emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/ANOTHER_LICENSE"}
                , emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/baz/two.txt"}
                , emptyLicenseUnitData{licenseUnitDataPath = "foo/bar/one.txt"}
                ]
    , licenseUnitFiles =
        "foo/bar/LICENSE"
          NE.:| [ "foo/bar/baz/ANOTHER_LICENSE"
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
