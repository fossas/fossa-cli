{-# LANGUAGE TemplateHaskell #-}

module Cargo.CargoTomlSpec (spec) where

import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Data.String.Conversion (toText)
import Path (Abs, Dir, Path, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Cargo (
  CargoProject (CargoProject),
  cargoDir,
  cargoToml,
 )
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, runIO)
import Types (License (..), LicenseResult (..), LicenseType (LicenseFile, LicenseSPDX, UnknownType))

spdxLicense :: License
spdxLicense =
  License
    { licenseType = LicenseSPDX
    , licenseValue = "MIT OR Apache-2.0"
    }

slashSpdxLicense :: License
slashSpdxLicense =
  License
    { licenseType = UnknownType
    , licenseValue = "MIT/Apache-2.0"
    }

cargoLicenseFile :: FilePath -> License
cargoLicenseFile filepath =
  License
    { licenseType = LicenseFile
    , licenseValue = toText filepath
    }

licenseResult :: [License] -> Path a Dir -> [LicenseResult]
licenseResult licenses baseDir =
  [ LicenseResult
      { licenseFile = toFilePath $ baseDir </> $(mkRelFile "Cargo.toml")
      , licensesFound = licenses
      }
  ]

cargoProject :: Path Abs Dir -> CargoProject
cargoProject baseDir =
  CargoProject
    { cargoDir = baseDir
    , cargoToml = baseDir </> $(mkRelFile "Cargo.toml")
    }

-- | The license-related fields for a Cargo.toml file are documented
--  [here](https://doc.rust-lang.org/cargo/reference/manifest.html#the-license-and-license-file-fields)
licenseSpecs :: Spec
licenseSpecs = do
  currentDir <- runIO getCurrentDir
  let mkBaseDir testDir = currentDir </> $(mkRelDir "test/Cargo/testdata") </> testDir
  describe "Cargo.toml LicenseAnalyzeProject" $ do
    it' "Reads a Cargo.toml with an SPDX license field" $ do
      let baseDir = mkBaseDir $(mkRelDir "spdx_cargo_toml")
      licenses <- licenseAnalyzeProject (cargoProject baseDir)
      licenses `shouldBe'` licenseResult [spdxLicense] baseDir

    it' "Reads a Cargo.toml without any license information" $ do
      let baseDir = mkBaseDir $(mkRelDir "missing_cargo_license")
      licenses <- licenseAnalyzeProject (cargoProject baseDir)
      licenses `shouldBe'` licenseResult [] baseDir

    it' "Reads a Cargo.toml with a license file" $ do
      let baseDir = mkBaseDir $(mkRelDir "license_file")
      let licenseFile = toFilePath (baseDir </> $(mkRelFile "LICENSE.txt"))
      licenses <- licenseAnalyzeProject (cargoProject baseDir)
      licenses `shouldBe'` licenseResult [cargoLicenseFile licenseFile] baseDir

    it' "Reads a Cargo.toml with both an SPDX license and a file" $ do
      let baseDir = mkBaseDir $(mkRelDir "spdx_license_and_file")
      let licenseFile = toFilePath (baseDir </> $(mkRelFile "LICENSE.txt"))
      licenses <- licenseAnalyzeProject (cargoProject baseDir)
      licenses
        `shouldBe'` licenseResult
          [ spdxLicense
          , cargoLicenseFile licenseFile
          ]
          baseDir

    it' "Reads a Cargo.toml with both SPDX licenses separated by '/'" $ do
      let baseDir = mkBaseDir $(mkRelDir "slash_licenses")
      licenses <- licenseAnalyzeProject (cargoProject baseDir)
      licenses `shouldBe'` licenseResult [slashSpdxLicense] baseDir

spec :: Spec
spec = licenseSpecs
