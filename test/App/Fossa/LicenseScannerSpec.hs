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
          [ standardData{licenseUnitDataPath = "foo/bar/one.txt"}
          , standardData{licenseUnitDataPath = "foo/bar/LICENSE"}
          ]
    , licenseUnitFiles =
        NE.fromList
          [ "foo/bar/one.txt"
          , "foo/bar/LICENSE"
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
          [ standardData{licenseUnitDataPath = "foo/bar/baz/two.txt"}
          , standardData{licenseUnitDataPath = "foo/bar/baz/ANOTHER_LICENSE"}
          ]
    , licenseUnitFiles =
        NE.fromList
          [ "foo/bar/baz/two.txt"
          , "foo/bar/baz/ANOTHER_LICENSE"
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
  describe "combineLicenseUnits" $
    it "should combine two units" $
      combineLicenseUnits [unitOne, unitTwo] `shouldBe` [expectedCombinedUnit]
