module R.RenvSpec (
  spec,
) where

import Data.Aeson (decodeFileStrict')
import Data.Map (fromList)
import Data.Text (Text)
import DepTypes (
  DepType (
    CranType,
    GitType,
    URLType
  ),
  Dependency (Dependency),
  VerConstraint (CEq),
 )
import GraphUtil (expectDeps, expectDirect, expectEdge)
import Strategy.R.Description (RDescription (RDescription))
import Strategy.R.Renv (
  RenvLock (RenvLock),
  RenvLockPackage (RenvLockPackage),
  RenvLockRepository (RenvLockRepository),
  RenvPackageSource (..),
  buildGraph,
  toDependency,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

spec :: Spec
spec = do
  describe "renv.lock" $ do
    it "should parse renv.lock file" $ do
      lockFile <- decodeFileStrict' "test/R/testdata/renv.lock"
      lockFile `shouldBe` Just expRenvLock

  describe "buildGraph" $ do
    it "should graph all packages as deep when description file is not present or empty" $ do
      let graph = buildGraph (RDescription mempty mempty mempty mempty mempty) fxLock
      expectDirect [] graph
      expectDeps [depA, depB, depC, depD] graph

    it "should graph packages in description file as direct" $ do
      let graph = buildGraph (RDescription ["a"] mempty mempty mempty mempty) fxLock
      expectDirect [depA] graph
      expectDeps [depA, depB, depC, depD] graph

    it "should graph package's requirements as edges" $ do
      let graph = buildGraph (RDescription mempty mempty mempty mempty mempty) fxLock
      expectDirect [] graph
      expectDeps [depA, depB, depC, depD] graph
      expectEdge graph depA depB
      expectEdge graph depC depD

  describe "toDependency" $ do
    it "should make git dependency for git source" $ do
      let dep = toDependency [] $ mkRPkg (Git "repoUrl" "ref")
      let expectedDep = mkDep GitType "repoUrl" (Just "ref")
      dep `shouldBe` expectedDep

    it "should make git dependency for github source" $ do
      let dep = toDependency [] $ mkRPkg (Github "user" "repo" "ref")
      let expectedDep = mkDep GitType "https://github.com/user/repo.git" (Just "ref")
      dep `shouldBe` expectedDep

    it "should make git dependency for gitlab source" $ do
      let dep = toDependency [] $ mkRPkg (Gitlab "user" "repo" "ref")
      let expectedDep = mkDep GitType "https://gitlab.com/user/repo.git" (Just "ref")
      dep `shouldBe` expectedDep

    it "should make git dependency for bitbucket source" $ do
      let dep = toDependency [] $ mkRPkg (Bitbucket "user" "repo" "ref")
      let expectedDep = mkDep GitType "https://bitbucket.com/user/repo.git" (Just "ref")
      dep `shouldBe` expectedDep

    it "should make url dependency for url source" $ do
      let dep = toDependency [] $ mkRPkg (Url "someUrl")
      let expectedDep = mkDep URLType "someUrl" Nothing
      dep `shouldBe` expectedDep

    it "should make cran dependency with repository url, when remote repo is found for cran" $ do
      let dep = toDependency [] $ mkRPkg (RemoteCran "someUrl")
      let expectedDep = mkDep CranType ("someUrl:" <> fxPkgId) (Just fxPkgVersion)
      dep `shouldBe` expectedDep

    it "should make cran dependency" $ do
      let dep = toDependency [] $ mkRPkg (Unknown)
      let expectedDep = mkDep CranType fxPkgId (Just fxPkgVersion)
      dep `shouldBe` expectedDep

expRenvLock :: RenvLock
expRenvLock =
  RenvLock
    [ RenvLockRepository "CRAN" "https://cran.rstudio.com"
    , RenvLockRepository "EXAMPLE-REPO" "https://EXAMPLE-REPO"
    ]
    $ fromList
      [ ("BiocManager", RenvLockPackage "BiocManager" "1.30.17" mempty $ Repository "CRAN")
      , ("emo", RenvLockPackage "emo" "0.0.0.9000" mempty $ Github "hadley" "emo" "3f03b11491ce3d6fc5601e210927eff73bf8e350")
      , ("nkbcgeneral", RenvLockPackage "nkbcgeneral" "0.8.0" mempty $ Bitbucket "cancercentrum" "nkbcgeneral" "db8e9206e7be44c53c5e7017900f38d506cf08a6")
      , ("BiocGenerics", RenvLockPackage "BiocGenerics" "0.40.0" mempty $ Git "https://git.bioconductor.org/packages/BiocGenerics" "RELEASE_3_14")
      , ("R6", RenvLockPackage "R6" "2.5.1" mempty $ Repository "EXAMPLE-REPO")
      , ("SOME-GITLAB-PKG", RenvLockPackage "some-gitlab-pkg" "0" mempty $ Gitlab "someUser" "someRepo" "someSha")
      , ("SOME-URL-PKG", RenvLockPackage "some-url-pkg" "1" mempty $ Url "https://some-url")
      , ("SOME-REMOTE-REF-PKG", RenvLockPackage "some-remote-ref-pkg" "2" mempty $ RemoteCran "http://some-custom-repo")
      ]

fxLock :: RenvLock
fxLock =
  RenvLock [] $
    fromList
      [ ("a", pkgA)
      , ("b", pkgB)
      , ("c", pkgC)
      , ("d", pkgD)
      ]

pkgA :: RenvLockPackage
pkgA = mkRPkg' "a" "0" ["b"]

depA :: Dependency
depA = toDependency [] pkgA

pkgB :: RenvLockPackage
pkgB = mkRPkg' "b" "1" []

depB :: Dependency
depB = toDependency [] pkgB

pkgC :: RenvLockPackage
pkgC = mkRPkg' "c" "2" ["d"]

depC :: Dependency
depC = toDependency [] pkgC

pkgD :: RenvLockPackage
pkgD = mkRPkg' "d" "3" []

depD :: Dependency
depD = toDependency [] pkgD

fxPkgId :: Text
fxPkgId = "a"

fxPkgVersion :: Text
fxPkgVersion = "0"

mkRPkg' :: Text -> Text -> [Text] -> RenvLockPackage
mkRPkg' pkg version reqs = RenvLockPackage pkg version reqs Unknown

mkRPkg :: RenvPackageSource -> RenvLockPackage
mkRPkg = RenvLockPackage fxPkgId fxPkgVersion mempty

mkDep :: DepType -> Text -> Maybe Text -> Dependency
mkDep dType dName dVersion =
  Dependency
    dType
    dName
    (CEq <$> dVersion)
    mempty
    mempty
    mempty
