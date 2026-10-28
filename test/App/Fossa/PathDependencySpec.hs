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
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvTesting),
  DepType (..),
  Dependency (..),
 )
import Effect.ReadFS (readContentsToml, resolveDir, resolveFile)
import Fossa.API.Types (AnalyzedPathDependency (AnalyzedPathDependency), Organization (..), PathDependencyUpload (..), UploadedPathDependencyLocator (..))
import Graphing (Graphing, direct, vertexList)
import Path
import Path.Extra (SomeResolvedPath (..))
import Path.IO qualified as PIO
import Srclib.Types (Locator (Locator))
import Strategy.Dart.PubSpecLock (analyzePubLockFile)
import Strategy.Python.Poetry (graphFromPyProjectAndLockFile)
import Strategy.Swift.PackageSwift (analyzePackageSwift)
import Test.Effect (EffectStack, it', shouldBe', shouldMatchList', shouldSatisfy')
import Test.Fixtures qualified as Fixtures
import Test.Hspec
import Test.MockApi (MockApi, alwaysReturns, alwaysReturnsForAnyRequest, returnsOnce, returnsOnceForAnyRequest)
import Types (DiscoveredProjectType (..), GraphBreadth (..), VerConstraint (..))

spec :: Spec
spec = do
  hashSpec
  absPathOfSpec
  enrichPathDependenciesSpec
  pathDependencyResolutionSpec

-- | End-to-end-style resolution tests for the three new path-dependency
-- ecosystems (Poetry, Swift/SwiftPM, Dart/pub).
--
-- Each test builds a graph the way the corresponding strategy builds it, from a
-- fixture project directory on disk (containing a manifest and/or lockfile that
-- declares a resolvable directory path dep and an unresolvable one), then runs
-- the full resolution layer, 'enrichPathDependencies', and asserts that the
-- resolvable dep is resolved (no longer 'UnresolvedPathType', with the uploaded
-- name/version) while the unresolvable one is skipped (left 'UnresolvedPathType')
-- and nothing crashes.
pathDependencyResolutionSpec :: Spec
pathDependencyResolutionSpec = do
  cwd <- runIO PIO.getCurrentDir
  poetryResolutionSpec cwd
  swiftResolutionSpec cwd
  dartResolutionSpec cwd

-- | The fixture project directories, relative to the repo root (the test cwd).
poetryProjRel :: Text
poetryProjRel = "test/App/Fossa/PathDependency/testdata/poetry-proj"

swiftProjRel :: Text
swiftProjRel = "test/App/Fossa/PathDependency/testdata/swift-proj"

dartProjRel :: Text
dartProjRel = "test/App/Fossa/PathDependency/testdata/dart-proj"

-- | 'enrichPathDependencies' with --include-unused-deps so that every
-- unresolved path dependency is attempted regardless of its environment.
runResolve :: ProjectResult -> EffectStack ProjectResult
runResolve = enrichPathDependencies (toFlag IncludeAll True) noRescanOption Fixtures.projectRevision

poetryResolutionSpec :: Path Abs Dir -> Spec
poetryResolutionSpec cwd =
  describe "Poetry path dependency resolution" $ do
    it' "resolves a directory path dep and skips an unresolvable one" $ do
      expectOrg orgSupportPathDeps
      expectPathDependencyScanUrlForAnyRequest "my-lib" "1.0.0"
      expectLicenseScanUploadForAnyRequest
      expectPathDependencyFinalizeForAnyRequest
      expectNoAnalyzedPathRevisions

      -- Build the graph the way the Poetry strategy builds it: from the pyproject
      -- and poetry.lock on disk. The fixture declares a resolvable ("lib") and an
      -- unresolvable ("does-not-exist") directory path dependency.
      projDir <- resolveDir cwd poetryProjRel
      pyprojectPath <- resolveFile cwd (poetryProjRel <> "/pyproject.toml")
      lockPath <- resolveFile cwd (poetryProjRel <> "/poetry.lock")
      graph <-
        graphFromPyProjectAndLockFile
          <$> readContentsToml pyprojectPath
          <*> readContentsToml lockPath
      let pr = mkProjectResult projDir graph
      result' <- runResolve pr
      let graph' = projectResultGraph result'

      -- The resolvable path dep is now resolved (PathType, uploaded name/version).
      depWithType PathType "my-lib" graph' `shouldBe'` True
      -- The unresolvable path dep is skipped and remains UnresolvedPathType.
      depWithType UnresolvedPathType "does-not-exist" graph' `shouldBe'` True
      -- Only the skipped dep remains unresolved.
      (unresolvedPathDepNames graph') `shouldMatchList'` ["does-not-exist"]

swiftResolutionSpec :: Path Abs Dir -> Spec
swiftResolutionSpec cwd =
  describe "Swift/SwiftPM path dependency resolution" $ do
    it' "resolves a directory path dep and skips an unresolvable one" $ do
      expectOrg orgSupportPathDeps
      expectPathDependencyScanUrlForAnyRequest "swift-lib" "2.0.0"
      expectLicenseScanUploadForAnyRequest
      expectPathDependencyFinalizeForAnyRequest
      expectNoAnalyzedPathRevisions

      -- Build the graph the way the SwiftPM strategy builds it: from Package.swift
      -- (+ Package.resolved) on disk. The manifest declares a resolvable ("lib")
      -- and an unresolvable ("does-not-exist") directory path dependency.
      projDir <- resolveDir cwd swiftProjRel
      manifestPath <- resolveFile cwd (swiftProjRel <> "/Package.swift")
      resolvedPath <- resolveFile cwd (swiftProjRel <> "/Package.resolved")
      graph <- analyzePackageSwift manifestPath (Just resolvedPath)
      let pr = mkProjectResult projDir graph
      result' <- runResolve pr
      let graph' = projectResultGraph result'

      depWithType PathType "swift-lib" graph' `shouldBe'` True
      depWithType UnresolvedPathType "does-not-exist" graph' `shouldBe'` True
      (unresolvedPathDepNames graph') `shouldMatchList'` ["does-not-exist"]

dartResolutionSpec :: Path Abs Dir -> Spec
dartResolutionSpec cwd =
  describe "Dart/pub path dependency resolution" $ do
    it' "resolves a directory path dep and skips an unresolvable one" $ do
      expectOrg orgSupportPathDeps
      expectPathDependencyScanUrlForAnyRequest "dart-lib" "3.0.0"
      expectLicenseScanUploadForAnyRequest
      expectPathDependencyFinalizeForAnyRequest
      expectNoAnalyzedPathRevisions

      -- Build the graph the way the Dart strategy builds it: from pubspec.lock on
      -- disk. The lock declares a resolvable ("lib") and an unresolvable
      -- ("does-not-exist") directory path dependency.
      projDir <- resolveDir cwd dartProjRel
      lockPath <- resolveFile cwd (dartProjRel <> "/pubspec.lock")
      (graph, _) <- analyzePubLockFile lockPath
      let pr = mkProjectResult projDir graph
      result' <- runResolve pr
      let graph' = projectResultGraph result'

      depWithType PathType "dart-lib" graph' `shouldBe'` True
      depWithType UnresolvedPathType "does-not-exist" graph' `shouldBe'` True
      (unresolvedPathDepNames graph') `shouldMatchList'` ["does-not-exist"]

-- | True if the graph contains a dependency with the given type and name.
depWithType :: DepType -> Text -> Graphing Dependency -> Bool
depWithType dt n g =
  any (\d -> dependencyType d == dt && dependencyName d == n) (Graphing.vertexList g)

-- | Names of the dependencies that are still unresolved path dependencies.
unresolvedPathDepNames :: Graphing Dependency -> [Text]
unresolvedPathDepNames =
  map dependencyName . filter (\d -> dependencyType d == UnresolvedPathType) . Graphing.vertexList

-- | API expectations shared by the ecosystem resolution tests.

-- | The scan-url response is keyed on a content hash we do not want to hardcode;
-- 'alwaysReturnsForAnyRequest' matches regardless of the requested path/hash.
expectPathDependencyScanUrlForAnyRequest :: Has MockApi sig m => Text -> Text -> m ()
expectPathDependencyScanUrlForAnyRequest name version =
  (GetPathDependencyScanUrl (PackageRevision "lib" "hash") Fixtures.projectRevision FileUploadMatchData)
    `alwaysReturnsForAnyRequest` PathDependencyUpload Fixtures.signedUrl (UploadedPathDependencyLocator name version)

expectLicenseScanUploadForAnyRequest :: Has MockApi sig m => m ()
expectLicenseScanUploadForAnyRequest =
  UploadLicenseScanResult Fixtures.signedUrl Fixtures.firstLicenseSourceUnit `alwaysReturnsForAnyRequest` ()

expectPathDependencyFinalizeForAnyRequest :: Has MockApi sig m => m ()
expectPathDependencyFinalizeForAnyRequest =
  FinalizeLicenseScanForPathDependency [Locator "path" "lib" (Just "1.0.0")] False `alwaysReturnsForAnyRequest` ()

absPathOfSpec :: Spec
absPathOfSpec = describe "absPathOfSpec" $ do
  cwd <- runIO PIO.getCurrentDir
  mkPathSpec cwd "../" isAbsDir
  mkPathSpec cwd "./" isAbsDir

  let currentPathRel = "../" <> (toText . toFilePath . dirname $ cwd)
  mkPathSpec cwd currentPathRel isAbsDir
  mkPathSpec cwd (currentPathRel <> "/") isAbsDir
  mkPathSpec cwd (currentPathRel <> "/test") isAbsDir
  mkPathSpec cwd (currentPathRel <> "/test/") isAbsDir

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

  it' "should skip unresolvable path dependencies, without aborting the enrichment pass" $ do
    expectOrg orgSupportPathDeps
    expectPathDependencyMatchData
    expectLicenseScanUpload
    expectPathDependencyFinalize
    expectNoAnalyzedPathRevisions

    let result = mkProjectResult cwd (graphWithUnPathDep <> direct unresolvablePathDep)
    let expectedResult = mkProjectResult cwd (direct resolvedPathDep <> direct unresolvablePathDep)

    result' <- enrichPathDependencies includeAll noRescanOption pr result
    result' `shouldBe'` expectedResult

  it' "should skip single-file path dependencies, without aborting the enrichment pass" $ do
    expectOrg orgSupportPathDeps
    expectPathDependencyMatchData
    expectLicenseScanUpload
    expectPathDependencyFinalize
    expectNoAnalyzedPathRevisions

    let result = mkProjectResult cwd (graphWithUnPathDep <> direct filePathDep)
    let expectedResult = mkProjectResult cwd (direct resolvedPathDep <> direct filePathDep)

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

unresolvablePathDep :: Dependency
unresolvablePathDep = (mkDep UnresolvedPathType){dependencyName = "does-not-exist"}

filePathDep :: Dependency
filePathDep = (mkDep UnresolvedPathType){dependencyName = fixtureFile}

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
