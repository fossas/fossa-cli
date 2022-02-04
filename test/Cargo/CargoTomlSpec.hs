{-# LANGUAGE TemplateHaskell #-}

-- |
--Module : Cargo.CargoTomlSpec
--Description : Tests for parsing Cargo.toml files
module Cargo.CargoTomlSpec (spec) where

import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Path (Abs, Dir, Path, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Cargo (
  CargoProject (CargoProject),
  cargoDir,
  cargoToml,
 )
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, runIO, describe)
import Types (License (..), LicenseResult (..), LicenseType (LicenseSPDX))

spdxLicense :: [License]
spdxLicense =
  [ License
      { licenseType = LicenseSPDX
      , licenseValue = "MIT OR Apache-2.0"
      }
  ]

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

-- |The license-related fields for a Cargo.toml file are documented
-- [here](https://doc.rust-lang.org/cargo/reference/manifest.html#the-license-and-license-file-fields)
licenseSpecs :: Spec
licenseSpecs = do
  currentDir <- runIO getCurrentDir
  describe "Cargo.toml LicenseAnalyzeProject" $ do
    it' "Reads a Cargo.toml with an SPDX license field" $ do
      let baseDir = currentDir </> $(mkRelDir "test/Cargo/testdata/spdx_cargo_toml")
      licenses <- licenseAnalyzeProject (cargoProject baseDir)
      licenses `shouldBe'` licenseResult spdxLicense baseDir

    it' "Reads a Cargo.toml without any license information" $ do
      let baseDir = currentDir </> $(mkRelDir "test/Cargo/testdata/missing_cargo_license")
      licenses <- licenseAnalyzeProject (cargoProject baseDir)
      licenses `shouldBe'` licenseResult [] baseDir

spec :: Spec
spec = licenseSpecs
