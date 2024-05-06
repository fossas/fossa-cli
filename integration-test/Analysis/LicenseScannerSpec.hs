{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.LicenseScannerSpec (spec) where

import Analysis.FixtureUtils (
  FixtureArtifact (FixtureArtifact),
  getArtifact,
 )
import App.Fossa.LicenseScanner (scanVendoredDep)
import App.Fossa.VendoredDependency (VendoredDependency (VendoredDependency))
import App.Types (FileUpload (..))
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (ignoreStickyLogger)
import Data.List.Extra (head')
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Diag.Result (Result (Failure, Success), renderFailure)
import Effect.Exec (runExecIO)
import Effect.ReadFS (runReadFSIO)
import Path (reldir, (</>))
import Path.IO qualified as PIO
import Srclib.Types (
  LicenseSourceUnit (licenseSourceUnitLicenseUnits),
  LicenseUnit (licenseUnitData, licenseUnitFiles, licenseUnitName),
  LicenseUnitData (licenseUnitDataContents, licenseUnitDataMatchData),
  LicenseUnitMatchData (licenseUnitMatchDataMatchString),
  emptyLicenseUnit,
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)
import Types (GlobFilter (GlobFilter), LicenseScanPathFilters (..))

recursiveArchive :: FixtureArtifact
recursiveArchive =
  FixtureArtifact
    "https://github.com/fossas/cli-license-scan-integration-test-fixtures/archive/refs/heads/main.zip"
    [reldir|manual-deps/license-scanner/|]
    [reldir|recursive-archive|]

vendoredDep :: VendoredDependency
vendoredDep =
  VendoredDependency
    "recursive-archive-test"
    "vendor/foo.tar.gz"
    (Just "0.0.1")

-- the contents of the archive look like this. The `FOO_LICENSE` files contain an MIT license.
-- `bar_apache.rb` and `something.rb` contain an Apache-2.0 license.
-- The quux directory is compressed into quux.tar.gz, and then baz into baz.tar.gz, bar into bar.tar.gz
-- and foo into foo.tar.gz
-- The script that creates this nested archive is here: https://github.com/fossas/example-projects/tree/main/recursive-archive-generator
--
-- recursive-archive
-- └── vendor
--     └── foo
--         ├── VERSION
--         └── bar
--             ├── MIT_LICENSE
--             ├── bar_apache.rb
--             └── baz
--                 ├── SOMETHING_LICENSE
--                 ├── quux
--                 │   └── QUUX_LICENSE
--                 └── something.rb

mitLicense :: Text
mitLicense =
  [r|Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.|]

spec :: Spec
spec = do
  describe "scanVendoredDep" $ do
    it "should find licenses in nested archives" $ do
      extractedDir <- getArtifact recursiveArchive
      let scanDir = extractedDir </> [reldir|cli-license-scan-integration-test-fixtures-main/recursive-archive|]
      units <- runStack . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO . fmap licenseSourceUnitLicenseUnits $ scanVendoredDep scanDir Nothing FileUploadMatchData vendoredDep
      PIO.removeDirRecur extractedDir
      case units of
        Failure ws eg -> fail (show (renderFailure ws eg "An issue occurred"))
        Success _ us -> do
          length us `shouldBe` 3
          NE.sort (NE.map licenseUnitName us) `shouldBe` NE.fromList ["No_license_found", "apache-2.0", "mit"]
          NE.sort (licenseUnitFiles mitUnit) `shouldBe` NE.fromList ["vendor/foo/bar/MIT_LICENSE", "vendor/foo/bar/baz/SOMETHING_LICENSE", "vendor/foo/bar/baz/quux/QUUX_LICENSE"]
          NE.sort (licenseUnitFiles apacheUnit) `shouldBe` NE.fromList ["vendor/foo/bar/bar_apache.rb", "vendor/foo/bar/baz/something.rb"]
          -- matchData should exist
          let matchData = concatMap NE.toList $ NE.toList (fromMaybe (NE.fromList []) . licenseUnitDataMatchData <$> licenseUnitData mitUnit)
          licenseUnitMatchDataMatchString <$> matchData `shouldBe` [Just mitLicense, Just mitLicense, Just mitLicense]
          -- no Contents since we're running themis with --srclib-with-matches
          licenseUnitDataContents <$> licenseUnitData mitUnit `shouldBe` NE.fromList [Nothing, Nothing, Nothing]
          where
            mitUnit :: LicenseUnit
            mitUnit = fromMaybe emptyLicenseUnit (head' $ NE.filter (\u -> licenseUnitName u == "mit") us)
            apacheUnit :: LicenseUnit
            apacheUnit = fromMaybe emptyLicenseUnit (head' $ NE.filter (\u -> licenseUnitName u == "apache-2.0") us)

    it "should get full file contents from Themis if full file uploads enabled" $ do
      extractedDir <- getArtifact recursiveArchive
      let scanDir = extractedDir </> [reldir|cli-license-scan-integration-test-fixtures-main/recursive-archive|]
      units <- runStack . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO . fmap licenseSourceUnitLicenseUnits $ scanVendoredDep scanDir Nothing FileUploadFullContent vendoredDep
      PIO.removeDirRecur extractedDir
      case units of
        Failure ws eg -> fail (show (renderFailure ws eg "An issue occurred"))
        Success _ us -> do
          length us `shouldBe` 3
          NE.sort (NE.map licenseUnitName us) `shouldBe` NE.fromList ["No_license_found", "apache-2.0", "mit"]
          NE.sort (licenseUnitFiles mitUnit) `shouldBe` NE.fromList ["vendor/foo/bar/MIT_LICENSE", "vendor/foo/bar/baz/SOMETHING_LICENSE", "vendor/foo/bar/baz/quux/QUUX_LICENSE"]
          NE.sort (licenseUnitFiles apacheUnit) `shouldBe` NE.fromList ["vendor/foo/bar/bar_apache.rb", "vendor/foo/bar/baz/something.rb"]
          -- We should get Contents since we're running themis with --srclib-with-full-files
          licenseUnitDataContents <$> licenseUnitData mitUnit `shouldBe` NE.fromList [Just mitLicense, Just mitLicense, Just mitLicense]
          -- matchData should be all Nothing
          let matchData = concatMap NE.toList $ NE.toList (fromMaybe (NE.fromList []) . licenseUnitDataMatchData <$> licenseUnitData mitUnit)
          licenseUnitMatchDataMatchString <$> matchData `shouldBe` [Nothing, Nothing, Nothing]
          where
            mitUnit :: LicenseUnit
            mitUnit = fromMaybe emptyLicenseUnit (head' $ NE.filter (\u -> licenseUnitName u == "mit") us)
            apacheUnit :: LicenseUnit
            apacheUnit = fromMaybe emptyLicenseUnit (head' $ NE.filter (\u -> licenseUnitName u == "apache-2.0") us)

    it "should filter licenses if LicenseScanPathFilters is set" $ do
      extractedDir <- getArtifact recursiveArchive
      let scanDir = extractedDir </> [reldir|cli-license-scan-integration-test-fixtures-main/recursive-archive|]
      let licenseScanPathFilters = LicenseScanPathFilters{licenseScanPathFiltersOnly = [GlobFilter "**.rb"], licenseScanPathFiltersExclude = [], licenseScanPathFilterFileExclude = []}
      units <- runStack . runDiagnostics . ignoreStickyLogger . runExecIO . runReadFSIO . fmap licenseSourceUnitLicenseUnits $ scanVendoredDep scanDir (Just licenseScanPathFilters) FileUploadMatchData vendoredDep
      PIO.removeDirRecur extractedDir
      case units of
        Failure ws eg -> fail (show (renderFailure ws eg "An issue occurred"))
        Success _ us -> do
          length us `shouldBe` 1
          NE.sort (NE.map licenseUnitName us) `shouldBe` NE.fromList ["apache-2.0"]
          NE.sort (licenseUnitFiles apacheUnit) `shouldBe` NE.fromList ["vendor/foo/bar/bar_apache.rb", "vendor/foo/bar/baz/something.rb"]
          where
            apacheUnit :: LicenseUnit
            apacheUnit = fromMaybe emptyLicenseUnit (head' $ NE.filter (\u -> licenseUnitName u == "apache-2.0") us)
