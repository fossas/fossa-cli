{-# LANGUAGE TemplateHaskell #-}

module Graphing.EnrichSpec (spec) where

import App.Fossa.VendoredDependency (
  VendoredDependency (VendoredDependency),
 )
import App.Types (FullFileUploads (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (FinalizeLicenseScan, GetAnalyzedRevisions, GetApiOpts, GetOrganization, GetSignedLicenseScanUrl, UploadLicenseScanResult), PackageRevision (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (DepEnvironment (EnvDevelopment), DepType (ArchiveType, NodeJSType, PathType), Dependency (..), VerConstraint (CEq))
import Fossa.API.Types (Archive (Archive), ArchiveComponents (..))
import Graphing (directs, edges)
import Graphing.Enrich (pathToArchives)
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Srclib.Types (LicenseSourceUnit, Locator (Locator), renderLocator)
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, runIO)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce, returnsOnceForAnyRequest)

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/VendoredDependency/testdata/repo")

spec :: Spec
spec = do
  describe "enrich" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir

    it' "should transform path dependency into archive dependency" $ do
      expectGetApiOpts
      expectGetOrganization
      expectEverythingScannedAlready
      expectFinalizeScan [archiveFoo]
      res <- pathToArchives scanDir (directs [pathDepFoo])
      res `shouldBe'` (directs [archiveDepFoo])

    it' "should transform path dependency into archive dependency, with scanning if core does not know about the revisions" $ do
      expectGetApiOpts
      expectGetOrganization
      expectNothingScannedYet
      expectGetSignedUrl PackageRevision{packageName = "foo", packageVersion = "fbd355fe946fab6928e91f76585054a1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectFinalizeScan [archiveFoo]
      let graph = directs [pathDepFoo]
      expectFatal' $ pathToArchives scanDir graph

    it' "should preserve dependency environment" $ do
      expectGetApiOpts
      expectGetOrganization
      expectEverythingScannedAlready
      expectFinalizeScan [archiveFoo]
      res <-
        pathToArchives
          scanDir
          ( directs
              [ mkPathDep
                  "foo"
                  "vendored/foo"
                  (Set.singleton EnvDevelopment)
                  mempty
              ]
          )
      res
        `shouldBe'` ( directs
                        [ mkArchiveDep
                            "42/foo"
                            "fbd355fe946fab6928e91f76585054a1"
                            "vendored/foo"
                            (Set.singleton EnvDevelopment)
                            mempty
                        ]
                    )

    it' "should preserve dependency edges" $ do
      expectGetApiOpts
      expectGetOrganization
      expectEverythingScannedAlready
      expectFinalizeScan [archiveFoo]
      -- -
      -- A -> path+foo -> B
      let graph = directs [npmA] <> edges [(npmA, pathDepFoo), (pathDepFoo, npmB)]
      -- -
      -- A -> archive+42/foo -> B
      let graph' = directs [npmA] <> edges [(npmA, archiveDepFoo), (archiveDepFoo, npmB)]

      result <- pathToArchives scanDir graph
      result `shouldBe'` graph'

    it' "should return supplied graph if there are no paths dependencies in the graph" $ do
      let graph = directs [npmA]
      result <- pathToArchives scanDir graph
      result `shouldBe'` graph

    it' "should fail if there are path dependencies without dependency locations" $ do
      let graph = directs [pathDepEvil]
      expectFatal' $ pathToArchives scanDir graph

-- Npm Dependency Fixture

npmA :: Dependency
npmA = mkNpmDep "a" "0.0.1"

npmB :: Dependency
npmB = mkNpmDep "b" "0.0.1"

mkNpmDep :: Text -> Text -> Dependency
mkNpmDep name ver = Dependency NodeJSType name (Just $ CEq ver) mempty mempty mempty

-- Path Dependency Fixture

pathDepFoo :: Dependency
pathDepFoo = mkPathDep "foo" "vendored/foo" mempty mempty

pathDepEvil :: Dependency
pathDepEvil = Dependency PathType "evil" Nothing mempty mempty mempty

mkPathDep :: Text -> Text -> Set DepEnvironment -> Map Text [Text] -> Dependency
mkPathDep name loc = Dependency PathType name Nothing [loc]

-- Archive Dependency Fixtures

archiveDepFoo :: Dependency
archiveDepFoo = mkArchiveDep "42/foo" "fbd355fe946fab6928e91f76585054a1" "vendored/foo" mempty mempty

mkArchiveDep :: Text -> Text -> Text -> Set DepEnvironment -> Map Text [Text] -> Dependency
mkArchiveDep name ver loc = Dependency ArchiveType name (Just $ CEq ver) [loc]

-- Vendor Dependency

vendorDepFoo :: VendoredDependency
vendorDepFoo = mkVendorDep "foo" "vendored/foo" "fbd355fe946fab6928e91f76585054a1"

mkVendorDep :: Text -> Text -> Text -> VendoredDependency
mkVendorDep name path ver = VendoredDependency name path (Just ver)

-- Locator

locatorFoo :: Locator
locatorFoo = mkLocator "foo" "fbd355fe946fab6928e91f76585054a1"

mkLocator :: Text -> Text -> Locator
mkLocator name ver = Locator "archive" ("42" <> "/" <> name) (Just ver)

-- Archives

archiveFoo :: Archive
archiveFoo = mkArchive "foo" "fbd355fe946fab6928e91f76585054a1"

mkArchive :: Text -> Text -> Archive
mkArchive = Archive

-- API Expectations

expectEverythingScannedAlready :: Has MockApi sig m => m ()
expectEverythingScannedAlready = GetAnalyzedRevisions (NE.fromList [vendorDepFoo]) `returnsOnce` map renderLocator [locatorFoo]

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts = GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectFinalizeScan :: Has MockApi sig m => [Archive] -> m ()
expectFinalizeScan as = (FinalizeLicenseScan ArchiveComponents{archives = as, forceRebuild = False, fullFiles = (FullFileUploads False)}) `returnsOnce` ()

expectNothingScannedYet :: Has MockApi sig m => m ()
expectNothingScannedYet = GetAnalyzedRevisions (NE.fromList [vendorDepFoo]) `returnsOnce` []

expectGetSignedUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedUrl packageRevision = GetSignedLicenseScanUrl packageRevision `alwaysReturns` Fixtures.signedUrl

expectUploadLicenseScanResult :: Has MockApi sig m => LicenseSourceUnit -> m ()
expectUploadLicenseScanResult licenseUnit =
  (UploadLicenseScanResult Fixtures.signedUrl licenseUnit) `returnsOnceForAnyRequest` ()
