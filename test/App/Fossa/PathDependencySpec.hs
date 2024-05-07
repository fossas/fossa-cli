module App.Fossa.PathDependencySpec (
  spec,
)
where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.Config.Analyze (IncludeAll (..), VendoredDependencyOptions (VendoredDependencyOptions))
import App.Fossa.PathDependency
import App.Types (FileUpload (..))
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClientF (..), PackageRevision (PackageRevision))
import Data.Flag (toFlag)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvTesting),
  DepType (..),
  Dependency (..),
 )
import Fossa.API.Types (AnalyzedPathDependency (AnalyzedPathDependency), Organization (..), PathDependencyUpload (..), UploadedPathDependencyLocator (..))
import Graphing (Graphing, direct)
import Path
import Path.Extra (SomeResolvedPath (..))
import Path.IO qualified as PIO
import Srclib.Types (Locator (Locator))
import Test.Effect (it', shouldBe', shouldSatisfy')
import Test.Fixtures qualified as Fixtures
import Test.Hspec
import Test.MockApi (MockApi, alwaysReturns, returnsOnce, returnsOnceForAnyRequest)
import Types (DiscoveredProjectType (..), GraphBreadth (..), VerConstraint (..))

spec :: Spec
spec = do
  hashSpec
  absPathOfSpec
  enrichPathDependenciesSpec

absPathOfSpec :: Spec
absPathOfSpec = describe "absPathOfSpec" $ do
  cwd <- runIO PIO.getCurrentDir
  mkPathSpec cwd "../" isAbsDir
  mkPathSpec cwd "./" isAbsDir

  mkPathSpec cwd "../fossa-cli" isAbsDir
  mkPathSpec cwd "../fossa-cli/" isAbsDir
  mkPathSpec cwd "../fossa-cli/test" isAbsDir
  mkPathSpec cwd "../fossa-cli/test/" isAbsDir

  mkPathSpec cwd "./test" isAbsDir
  mkPathSpec cwd "./test/" isAbsDir

  mkPathSpec cwd "./test/test.hs" isAbsFile
  mkPathSpec cwd "test/test.hs" isAbsFile

  mkPathSpec cwd "./Changelog.md" isAbsFile
  mkPathSpec cwd "Changelog.md" isAbsFile

hashSpec :: Spec
hashSpec = describe "hash" $ do
  cwd <- runIO PIO.getCurrentDir

  it' "should hash directory" $ do
    fixtureDir' <- absPathOf cwd fixtureDir
    hash <- hashOf fixtureDir'
    hash `shouldBe'` fixtureDirHash

  it' "should hash empty file" $ do
    emptyFile' <- absPathOf cwd emptyFile
    hash <- hashOf emptyFile'
    hash `shouldBe'` "d41d8cd98f00b204e9800998ecf8427e"

  it' "should hash file" $ do
    fixtureFile' <- absPathOf cwd fixtureFile
    hash <- hashOf fixtureFile'
    hash `shouldBe'` "6eee9de91973024bafcdeb60aed2a0c8"

isAbsDir :: SomeResolvedPath -> Bool
isAbsDir (ResolvedDir _) = True
isAbsDir (ResolvedFile _) = False

isAbsFile :: SomeResolvedPath -> Bool
isAbsFile = not . isAbsDir

mkPathSpec :: Path Abs Dir -> Text -> (SomeResolvedPath -> Bool) -> SpecWith ()
mkPathSpec cwd path comp =
  it' "should resolve absolute path" $ do
    path' <- absPathOf cwd path
    path' `shouldSatisfy'` comp

emptyFile :: Text
emptyFile = "test/App/Fossa/PathDependency/testdata/emptyfile.txt"

fixtureFile :: Text
fixtureFile = "test/App/Fossa/PathDependency/testdata/example.txt"

fixtureDir :: Text
fixtureDir = "test/App/Fossa/PathDependency/testdata/example"

fixtureDirHash :: Text
fixtureDirHash = "026ab8bcf9ac68ae7ded65922f8f83c8"

-- Api

enrichPathDependenciesSpec :: Spec
enrichPathDependenciesSpec = describe "enrichPathDependencies" $ do
  cwd <- runIO PIO.getCurrentDir
  let pr = Fixtures.projectRevision
  let includeAll = toFlag IncludeAll True
  let notIncludeAll = toFlag IncludeAll False

  it' "should not perform path dependency enrichment, if org does not support it" $ do
    expectGetApiOpts
    expectOrg orgDoesNotSupportPathDeps

    let result = mkProjectResult cwd graphWithUnPathDep
    result' <- enrichPathDependencies includeAll noRescanOption pr result
    result' `shouldBe'` result

  it' "should not perform path dependency enrichment, if result has no unresolved deps" $ do
    expectGetApiOpts
    expectOrg orgSupportPathDeps

    let result = mkProjectResult cwd graphWithPathDep
    result' <- enrichPathDependencies includeAll noRescanOption pr result
    result' `shouldBe'` result

  it' "should perform path dependency scan and upload, if path dependency is not already analyzed" $ do
    expectOrg orgSupportPathDeps
    expectPathDependencyMatchData
    expectLicenseScanUpload
    expectPathDependencyFinalize
    expectNoAnalyzedPathRevisions

    let result = mkProjectResult cwd graphWithUnPathDep
    let expectedResult = mkProjectResult cwd . direct $ resolvedPathDep

    result' <- enrichPathDependencies includeAll noRescanOption pr result
    result' `shouldBe'` expectedResult

  it' "should not perform path dependency scan and upload, if path dependency is already analyzed" $ do
    expectOrg orgSupportPathDeps{orgRequiresFullFileUploads = False}
    expectAnalyzedPathRevisions

    let result = mkProjectResult cwd graphWithUnPathDep
    let expectedResult = mkProjectResult cwd . direct $ resolvedPathDep

    result' <- enrichPathDependencies includeAll noRescanOption pr result
    result' `shouldBe'` expectedResult

  describe "includeAll [--include-unused-deps]" $ do
    it' "should not perform enrichment for non-production path deps" $ do
      expectGetApiOpts
      expectOrg orgSupportPathDeps

      let result = mkProjectResult cwd (direct nonProdUPathDepOnly)
      result' <- enrichPathDependencies notIncludeAll noRescanOption pr result
      result' `shouldBe'` result

    it' "should perform enrichment for non-production path deps, when flag is included" $ do
      expectOrg orgSupportPathDeps{orgRequiresFullFileUploads = False}
      expectPathDependencyMatchData
      expectLicenseScanUpload
      expectPathDependencyFinalize
      expectNoAnalyzedPathRevisions

      let result = mkProjectResult cwd (direct nonProdUPathDepOnly)
      let expectedResult =
            mkProjectResult cwd . direct $
              nonProdUPathDepOnly
                { dependencyType = PathType
                , dependencyName = "1"
                , dependencyVersion = Just . CEq $ fixtureDirHash
                }

      result' <- enrichPathDependencies includeAll noRescanOption pr result
      result' `shouldBe'` expectedResult

  describe "fullFile" $ do
    it' "should upload with fullFile, if org requires fullfile" $ do
      expectOrg orgRequiresFullFile
      expectPathDependencyFullFile
      expectLicenseScanUpload
      expectPathDependencyFinalize
      expectNoAnalyzedPathRevisions

      let result = mkProjectResult cwd graphWithUnPathDep
      let expectedResult = mkProjectResult cwd . direct $ resolvedPathDep
      result' <- enrichPathDependencies includeAll noRescanOption pr result
      result' `shouldBe'` expectedResult

    it' "should upload with matchData, if org does not requires fullfile" $ do
      expectOrg orgSupportPathDeps
      expectPathDependencyMatchData
      expectLicenseScanUpload
      expectPathDependencyFinalize
      expectNoAnalyzedPathRevisions

      let result = mkProjectResult cwd graphWithUnPathDep
      let expectedResult = mkProjectResult cwd . direct $ resolvedPathDep
      result' <- enrichPathDependencies includeAll noRescanOption pr result
      result' `shouldBe'` expectedResult

  describe "forceRescan" $ do
    it' "should not retrieve analyzed versions, if forceRescan is true" $ do
      expectOrg orgRequiresFullFile
      expectPathDependencyFullFile
      expectLicenseScanUpload
      expectPathDependencyFinalize

      let result = mkProjectResult cwd graphWithUnPathDep
      let expectedResult = mkProjectResult cwd . direct $ resolvedPathDep
      result' <- enrichPathDependencies includeAll forceRescanOption pr result
      result' `shouldBe'` expectedResult

    it' "should retrieve analyzed versions, if forceRescan is false" $ do
      expectOrg orgSupportPathDeps
      expectPathDependencyMatchData
      expectLicenseScanUpload
      expectPathDependencyFinalize
      expectNoAnalyzedPathRevisions

      let result = mkProjectResult cwd graphWithUnPathDep
      let expectedResult = mkProjectResult cwd . direct $ resolvedPathDep
      result' <- enrichPathDependencies includeAll noRescanOption pr result
      result' `shouldBe'` expectedResult

forceRescanOption :: VendoredDependencyOptions
forceRescanOption = VendoredDependencyOptions True Nothing Nothing

noRescanOption :: VendoredDependencyOptions
noRescanOption = VendoredDependencyOptions False Nothing Nothing

mkDep :: DepType -> Dependency
mkDep dt = Dependency dt fixtureDir Nothing mempty mempty mempty

nonProdUPathDepOnly :: Dependency
nonProdUPathDepOnly =
  (mkDep UnresolvedPathType)
    { dependencyEnvironments = Set.singleton EnvTesting
    }

resolvedPathDep :: Dependency
resolvedPathDep = (mkDep PathType){dependencyName = "1", dependencyVersion = Just . CEq $ fixtureDirHash}

graphWithUnPathDep :: Graphing Dependency
graphWithUnPathDep = direct $ mkDep UnresolvedPathType

graphWithPathDep :: Graphing Dependency
graphWithPathDep = direct $ mkDep PathType

mkProjectResult :: Path Abs Dir -> Graphing Dependency -> ProjectResult
mkProjectResult path deps =
  ProjectResult
    { projectResultType = YarnProjectType
    , projectResultPath = path
    , projectResultGraph = deps
    , projectResultGraphBreadth = Partial
    , projectResultManifestFiles = mempty
    }

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts = GetApiOpts `alwaysReturns` Fixtures.apiOpts

orgSupportPathDeps :: Organization
orgSupportPathDeps = Fixtures.organization{orgSupportsPathDependencyScans = True}

orgRequiresFullFile :: Organization
orgRequiresFullFile = Fixtures.organization{orgSupportsPathDependencyScans = True, orgRequiresFullFileUploads = True}

orgDoesNotSupportPathDeps :: Organization
orgDoesNotSupportPathDeps = Fixtures.organization{orgSupportsPathDependencyScans = False}

expectOrg :: Has MockApi sig m => Organization -> m ()
expectOrg org = GetOrganization `returnsOnce` org

expectPathDependencyFullFile :: Has MockApi sig m => m ()
expectPathDependencyFullFile =
  ( GetPathDependencyScanUrl
      (PackageRevision fixtureDir fixtureDirHash)
      (Fixtures.projectRevision)
      FileUploadFullContent
  )
    `returnsOnce` PathDependencyUpload (Fixtures.signedUrl) (UploadedPathDependencyLocator "1" fixtureDirHash)

expectPathDependencyMatchData :: Has MockApi sig m => m ()
expectPathDependencyMatchData =
  ( GetPathDependencyScanUrl
      (PackageRevision fixtureDir fixtureDirHash)
      (Fixtures.projectRevision)
      FileUploadMatchData
  )
    `returnsOnce` PathDependencyUpload (Fixtures.signedUrl) (UploadedPathDependencyLocator "1" fixtureDirHash)

expectLicenseScanUpload :: Has MockApi sig m => m ()
expectLicenseScanUpload = (UploadLicenseScanResult Fixtures.signedUrl Fixtures.firstLicenseSourceUnit) `returnsOnceForAnyRequest` ()

expectPathDependencyFinalize :: Has MockApi sig m => m ()
expectPathDependencyFinalize = (FinalizeLicenseScanForPathDependency [Locator "path" "1" $ Just fixtureDirHash]) False `returnsOnce` ()

expectNoAnalyzedPathRevisions :: Has MockApi sig m => m ()
expectNoAnalyzedPathRevisions = GetAnalyzedPathRevisions Fixtures.projectRevision `returnsOnce` []

expectAnalyzedPathRevisions :: Has MockApi sig m => m ()
expectAnalyzedPathRevisions = GetAnalyzedPathRevisions Fixtures.projectRevision `returnsOnce` [AnalyzedPathDependency fixtureDir "1" fixtureDirHash]
