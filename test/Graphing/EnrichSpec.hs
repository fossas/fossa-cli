{-# LANGUAGE TemplateHaskell #-}

module Graphing.EnrichSpec (spec) where

import App.Types (FullFileUploads (..))
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
import Graphing.Enrich (conanToArchives)
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Srclib.Types (LicenseSourceUnit)
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

    it' "should transform conan dependency into archive dependency" $ do
      expectGetApiOpts
      expectGetOrganization
      expectGetSignedUrl PackageRevision{packageName = "foo", packageVersion = "0.0.1"}
      expectUploadLicenseScanResult Fixtures.firstLicenseSourceUnit
      expectFinalizeScan [archiveFoo]

      let graph = directs [conanDepFoo]
      let graph' = directs [archiveDepFoo]

      res <- conanToArchives scanDir graph
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

      res <- conanToArchives scanDir graph
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

      result <- conanToArchives scanDir graph
      result `shouldBe'` graph'

    it' "should return supplied graph if there are no conan dependencies in the graph" $ do
      let graph = directs [npmA]
      result <- conanToArchives scanDir graph
      result `shouldBe'` graph

    it' "should fail if there are conan dependencies without dependency locations" $ do
      let graph = directs [conanDepEvil]
      expectFatal' $ conanToArchives scanDir graph

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
archiveFoo = mkArchive "foo" "0.0.1"

mkArchive :: Text -> Text -> Archive
mkArchive = Archive

-- API Expectations

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts = GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectFinalizeScan :: Has MockApi sig m => [Archive] -> m ()
expectFinalizeScan as = (FinalizeLicenseScan ArchiveComponents{archives = as, forceRebuild = False, fullFiles = (FullFileUploads False)}) `returnsOnce` ()

expectGetSignedUrl :: Has MockApi sig m => PackageRevision -> m ()
expectGetSignedUrl packageRevision = GetSignedLicenseScanUrl packageRevision `alwaysReturns` Fixtures.signedUrl

expectUploadLicenseScanResult :: Has MockApi sig m => LicenseSourceUnit -> m ()
expectUploadLicenseScanResult licenseUnit =
  (UploadLicenseScanResult Fixtures.signedUrl licenseUnit) `returnsOnceForAnyRequest` ()
