{-# LANGUAGE TemplateHaskell #-}

module Conan.EnrichSpec (spec) where

import App.Fossa.VendoredDependency (VendoredDependency (..))
import App.Types (DependencyRebuild (DependencyRebuildReuseCache), FileUpload (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (
  FossaApiClientF (
    FinalizeLicenseScan,
    GetApiOpts,
    GetOrganization,
    GetSignedLicenseScanUrl,
    UploadLicenseScanResult
  ),
  PackageRevision (..),
 )
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvDevelopment),
  DepType (ArchiveType, ConanType, NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Fossa.API.Types (Archive (Archive), ArchiveComponents (..))
import Graphing (directs, edges)
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Srclib.Types (LicenseSourceUnit, Locator (..))
import Strategy.Conan.Enrich (conanDepToVendorDep, conanToArchives, findArchiveTwin, locatorToArchiveDep)
import Test.Effect (expectFatal', it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.MockApi (MockApi, alwaysReturns, returnsOnce, returnsOnceForAnyRequest)

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/VendoredDependency/testdata/repo")

spec :: Spec
spec = do
  conanToVendoredDepSpec
  locatorToArchiveDepSpec
  findArchiveTwinSpec

  describe "enrich" $ do
    currDir <- runIO getCurrentDir
    let scanDir = currDir </> fixtureDir
    let conanToArchives' root = conanToArchives root FileUploadMatchData

    it' "should transform conan dependency into archive dependency" $ do
      expectGetApiOpts
      expectGetOrganization
      expectGetSignedUrl PackageRevision{packageName = "foo", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectFinalizeScan [archiveFoo]

      let graph = directs [conanDepFoo]
      let graph' = directs [archiveDepFoo]

      res <- conanToArchives' scanDir graph
      res `shouldBe'` graph'

    it' "should preserve dependency environment" $ do
      expectGetApiOpts
      expectGetOrganization
      expectGetSignedUrl PackageRevision{packageName = "foo", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectFinalizeScan [archiveFoo]

      let graph =
            directs
              [ mkConanDep
                  "foo"
                  "vendored/foo"
                  (Set.singleton EnvDevelopment)
                  mempty
              ]
      let graph' =
            directs
              [ mkArchiveDep
                  "42/foo"
                  "0.0.1"
                  "vendored/foo"
                  (Set.singleton EnvDevelopment)
                  mempty
              ]

      res <- conanToArchives' scanDir graph
      res `shouldBe'` graph'

    it' "should preserve dependency edges" $ do
      expectGetApiOpts
      expectGetOrganization
      expectGetSignedUrl PackageRevision{packageName = "foo", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectFinalizeScan [archiveFoo]

      -- -
      -- A -> conan+foo -> B
      let graph = directs [npmA] <> edges [(npmA, conanDepFoo), (conanDepFoo, npmB)]
      -- -
      -- A -> archive+42/foo -> B
      let graph' = directs [npmA] <> edges [(npmA, archiveDepFoo), (archiveDepFoo, npmB)]

      result <- conanToArchives' scanDir graph
      result `shouldBe'` graph'

    it' "should return supplied graph if there are no conan dependencies in the graph" $ do
      let graph = directs [npmA]
      result <- conanToArchives' scanDir graph
      result `shouldBe'` graph

    it' "should fail if there are conan dependencies without dependency locations" $ do
      let graph = directs [conanDepEvil]
      expectFatal' $ conanToArchives' scanDir graph

-- Npm Dependency Fixture

npmA :: Dependency
npmA = mkNpmDep "a" "0.0.1"

npmB :: Dependency
npmB = mkNpmDep "b" "0.0.1"

mkNpmDep :: Text -> Text -> Dependency
mkNpmDep name ver = Dependency NodeJSType name (Just $ CEq ver) mempty mempty mempty

-- Conan Dependency Fixture

conanDepFoo :: Dependency
conanDepFoo = mkConanDep "foo" "vendored/foo" mempty mempty

conanDepEvil :: Dependency
conanDepEvil = Dependency ConanType "evil" (Just $ CEq "0.0.1") mempty mempty mempty

mkConanDep :: Text -> Text -> Set DepEnvironment -> Map Text [Text] -> Dependency
mkConanDep name loc = Dependency ConanType name (Just $ CEq "0.0.1") [loc]

-- Archive Dependency Fixtures

archiveDepFoo :: Dependency
archiveDepFoo = mkArchiveDep "42/foo" "0.0.1" "vendored/foo" mempty mempty

mkArchiveDep :: Text -> Text -> Text -> Set DepEnvironment -> Map Text [Text] -> Dependency
mkArchiveDep name ver loc = Dependency ArchiveType name (Just $ CEq ver) [loc]

-- Archives

archiveFoo :: Archive
archiveFoo = mkArchive "foo" "0.0.1" Nothing Nothing

mkArchive :: Text -> Text -> Maybe Text -> Maybe Text -> Archive
mkArchive = Archive

-- API Expectations

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts = GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectFinalizeScan :: Has MockApi sig m => [Archive] -> m ()
expectFinalizeScan as = flip returnsOnce () . FinalizeLicenseScan $ ArchiveComponents as DependencyRebuildReuseCache FileUploadMatchData

expectGetSignedUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedUrl packageRevision = GetSignedLicenseScanUrl packageRevision `alwaysReturns` Fixtures.signedUrl

expectUploadLicenseScanResult :: Has MockApi sig m => LicenseSourceUnit -> m ()
expectUploadLicenseScanResult licenseUnit =
  (UploadLicenseScanResult Fixtures.signedUrl licenseUnit) `returnsOnceForAnyRequest` ()

conanToVendoredDepSpec :: Spec
conanToVendoredDepSpec =
  describe "conanToVendoredDep" $ do
    it "should transforms conan to vendor dep, when dep has location" $ do
      let res = conanDepToVendorDep conanDepFoo
      res `shouldBe` Right (conanDepFoo, VendoredDependency "foo" "vendored/foo" (Just "0.0.1") Nothing)

    it "should not transforms conan to vendor dep, when dep does not have location" $ do
      let dep = conanDepFoo{dependencyLocations = []}
      let res = conanDepToVendorDep dep
      res `shouldBe` (Left dep)

locatorToArchiveDepSpec :: Spec
locatorToArchiveDepSpec =
  describe "locatorToArchiveDep" $ do
    it "should transforms archive locator to vendor dep" $ do
      let loc = Locator "archive" "42/foo" $ Just "0.0.1"
      let res = locatorToArchiveDep [conanDepFoo] loc
      res `shouldBe` (Right archiveDepFoo)

    it "should not transforms non-archive locator to vendor dep" $ do
      let loc = Locator "pip" "foo" $ Just "1.0.0"
      let res = locatorToArchiveDep [conanDepFoo] loc
      res `shouldBe` (Left loc)

findArchiveTwinSpec :: Spec
findArchiveTwinSpec =
  describe "findArchiveTwin" $ do
    it "should find archive sibling for given conan dependency" $ do
      let res = findArchiveTwin [archiveDepFoo] conanDepFoo
      res `shouldBe` (Right (conanDepFoo, archiveDepFoo))

    it "should not find archive sibling for inequivalent conan dependency (dep name differs)" $ do
      let res = findArchiveTwin [archiveDepFoo] conanDepEvil
      res `shouldBe` (Left conanDepEvil)

    it "should not find archive sibling for inequivalent conan dependency (location differs)" $ do
      let dep' = conanDepFoo{dependencyLocations = []}
      let res = findArchiveTwin [archiveDepFoo] dep'
      res `shouldBe` (Left dep')
