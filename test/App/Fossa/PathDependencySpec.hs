module App.Fossa.PathDependencySpec (
  spec,
)
where

import Path (File, Path, Abs, File, Dir)
import Test.Effect (it', shouldBe')
import Test.Hspec
import Path.IO qualified as PIO
import App.Fossa.PathDependency

spec :: Spec
spec = hashSpec

hashSpec :: Spec
hashSpec = fdescribe "hash" $ do
  emptyDir' <- runIO emptyDir
  emptyFile' <- runIO emptyFile
  fixtureDir' <- runIO fixtureDir
  fixtureFile' <- runIO fixtureFile

  it' "should hash empty directory" $ do
    hash <- hashOf (SDir emptyDir')
    hash `shouldBe'` "76cdb2bad9582d23c1f6f4d868218d6c"

  it' "should hash directory" $ do
    hash <- hashOf (SDir fixtureDir')
    hash `shouldBe'` "20a2312b420bf049db1adcef93a3b48c"

  it' "should hash empty file" $ do
    hash <- hashOf (SFile emptyFile')
    hash `shouldBe'` "d41d8cd98f00b204e9800998ecf8427e"

  it' "should hash file" $ do
    hash <- hashOf (SFile fixtureFile')
    hash `shouldBe'` "6eee9de91973024bafcdeb60aed2a0c8"

emptyDir :: IO (Path Abs Dir)
emptyDir = PIO.resolveDir' "test/App/FOSSA/PathDependency/testdata/empty"

emptyFile ::  IO (Path Abs File)
emptyFile = PIO.resolveFile' "test/App/FOSSA/PathDependency/testdata/emptyfile.txt"

fixtureFile ::  IO (Path Abs File)
fixtureFile = PIO.resolveFile' "test/App/FOSSA/PathDependency/testdata/example.txt"

fixtureDir :: IO (Path Abs Dir)
fixtureDir = PIO.resolveDir' "test/App/FOSSA/PathDependency/testdata/example"