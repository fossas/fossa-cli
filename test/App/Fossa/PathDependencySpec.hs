module App.Fossa.PathDependencySpec (
  spec,
)
where

import App.Fossa.PathDependency
import Control.Algebra (Has)
import Data.Text (Text)
import Path (Abs, Dir, File, Path)
import Path.Extra (SomeResolvedPath (..))
import Path.IO qualified as PIO
import Test.Effect (it', shouldBe', shouldSatisfy')
import Test.Hspec
import Test.MockApi (MockApi)

spec :: Spec
spec = do
  hashSpec
  absPathOfSpec

-- enrichPathDependenciesSpec :: Spec
-- enrichPathDependenciesSpec = fdescribe "enrichPathDependencies" $ do
-- Path dependency NOOP
-- it' "should do nothing, if path dependency scanning is not supported!" $ do
-- it' "should do nothing, if there are no path dependencies!" $ do
-- it' "should do nothing, if there are no prod path dependencies!" $ do

-- Path dependency Scanning
-- it' "should perform enrichment!" $ do
-- it' "should perform enrichment!" $ do
-- it' "should scan all, if endpoint does not know about all path dependencies!" $ do

-- Path dependency Skipping
-- it' "should skip, endpoint knows about all path dependencies!" $ do
-- it' "should always scan all, if dependency skipping is not supported!" $ do
-- it' "should always scan all, if force dependency scanning flag is provided!" $ do

-- Scanning kind Flag
-- it' "should always upload with matchData flag if the org does not require full files!" $ do
-- it' "should always upload with fullFiles flag if the org requires full files!" $ do

-- FS specs

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
  emptyDir' <- runIO emptyDir
  emptyFile' <- runIO emptyFile
  fixtureDir' <- runIO fixtureDir
  fixtureFile' <- runIO fixtureFile

  it' "should hash empty directory" $ do
    hash <- hashOf (ResolvedDir emptyDir')
    hash `shouldBe'` "76cdb2bad9582d23c1f6f4d868218d6c"

  it' "should hash directory" $ do
    hash <- hashOf (ResolvedDir fixtureDir')
    hash `shouldBe'` "20a2312b420bf049db1adcef93a3b48c"

  it' "should hash empty file" $ do
    hash <- hashOf (ResolvedFile emptyFile')
    hash `shouldBe'` "d41d8cd98f00b204e9800998ecf8427e"

  it' "should hash file" $ do
    hash <- hashOf (ResolvedFile fixtureFile')
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

emptyDir :: IO (Path Abs Dir)
emptyDir = PIO.resolveDir' "test/App/FOSSA/PathDependency/testdata/empty"

emptyFile :: IO (Path Abs File)
emptyFile = PIO.resolveFile' "test/App/FOSSA/PathDependency/testdata/emptyfile.txt"

fixtureFile :: IO (Path Abs File)
fixtureFile = PIO.resolveFile' "test/App/FOSSA/PathDependency/testdata/example.txt"

fixtureDir :: IO (Path Abs Dir)
fixtureDir = PIO.resolveDir' "test/App/FOSSA/PathDependency/testdata/example"

-- Api Mocks

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts =
  GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = GetOrganization `alwaysReturns` Fixtures.organization

expectGetOrganization' :: Has MockApi sig m => m ()
expectGetOrganization' = GetOrganization `alwaysReturns` Fixtures.organization

expectNothingScannedYet :: Has MockApi sig m => m ()
expectNothingScannedYet = GetAnalyzedRevisions Fixtures.vendoredDeps `returnsOnce` []
