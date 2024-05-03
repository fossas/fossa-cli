module Python.Poetry.CommonSpec (
  spec,
) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import Strategy.Python.Poetry.Common (getPoetryBuildBackend, makePackageToLockDependencyMap, pyProjectDeps, supportedPoetryLockDep, supportedPyProjectDep, toCanonicalName)
import Strategy.Python.Poetry.PoetryLock (
  ObjectVersion (..),
  PackageName (..),
  PoetryLockDependencySpec (..),
  PoetryLockPackage (..),
  PoetryLockPackageSource (..),
 )
import Strategy.Python.Poetry.PyProject (
  PoetryDependency (..),
  PyProject (..),
  PyProjectBuildSystem (..),
  PyProjectPoetry (..),
  PyProjectPoetryDetailedVersionDependency (..),
  PyProjectPoetryGitDependency (..),
  PyProjectPoetryPathDependency (..),
  PyProjectPoetryUrlDependency (..),
  pyProjectCodec,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  runIO,
  shouldBe,
  shouldMatchList,
 )
import Toml qualified

expectedPyProject :: PyProject
expectedPyProject =
  PyProject
    { pyprojectBuildSystem = Just $ PyProjectBuildSystem{buildBackend = "poetry.core.masonry.api"}
    , pyprojectProject = Nothing
    , pyprojectPdmDevDependencies = Nothing
    , pyprojectPoetry =
        Just $
          PyProjectPoetry
            { name = Just "test_name"
            , version = Just "test_version"
            , description = Just "test_description"
            , dependencies =
                Map.fromList
                  [ ("flake8", PoetryTextVersion "^1.1")
                  , ("python", PoetryTextVersion "^3.9")
                  ,
                    ( "flask"
                    , PyProjectPoetryGitDependencySpec
                        PyProjectPoetryGitDependency
                          { gitUrl = "https://github.com/pallets/flask.git"
                          , gitRev = Just "38eb5d3b"
                          , gitTag = Nothing
                          , gitBranch = Nothing
                          }
                    )
                  ,
                    ( "networkx"
                    , PyProjectPoetryGitDependencySpec
                        PyProjectPoetryGitDependency
                          { gitUrl = "https://github.com/networkx/networkx.git"
                          , gitRev = Nothing
                          , gitTag = Nothing
                          , gitBranch = Nothing
                          }
                    )
                  ,
                    ( "numpy"
                    , PyProjectPoetryGitDependencySpec
                        PyProjectPoetryGitDependency
                          { gitUrl = "https://github.com/numpy/numpy.git"
                          , gitRev = Nothing
                          , gitTag = Just "v0.13.2"
                          , gitBranch = Nothing
                          }
                    )
                  ,
                    ( "requests"
                    , PyProjectPoetryGitDependencySpec
                        PyProjectPoetryGitDependency
                          { gitUrl = "https://github.com/kennethreitz/requests.git"
                          , gitRev = Nothing
                          , gitTag = Nothing
                          , gitBranch = Just "next"
                          }
                    )
                  , ("my-packageUrl", PyProjectPoetryUrlDependencySpec $ PyProjectPoetryUrlDependency "https://example.com/my-package-0.1.0.tar.gz")
                  , ("my-packageFile", PyProjectPoetryPathDependencySpec $ PyProjectPoetryPathDependency "../my-package/dist/my-package-0.1.0.tar.gz")
                  , ("my-packageDir", PyProjectPoetryPathDependencySpec $ PyProjectPoetryPathDependency "../my-package/")
                  , ("black", PyProjectPoetryDetailedVersionDependencySpec $ PyProjectPoetryDetailedVersionDependency "19.10b0")
                  ]
            , devDependencies =
                Map.fromList
                  [("pytest", PoetryTextVersion "*")]
            , groupDevDependencies = Map.empty
            , groupTestDependencies = Map.empty
            }
    }

notSupportedPyProjectDependency :: PoetryDependency
notSupportedPyProjectDependency = PyProjectPoetryPathDependencySpec $ PyProjectPoetryPathDependency{sourcePath = "../my-package/dist/my-package-0.1.0.tar.gz"}

notSupportedPoetryLockDependency :: PoetryLockPackage
notSupportedPoetryLockDependency =
  PoetryLockPackage
    { poetryLockPackageName = PackageName "pkgSourcedFromFile"
    , poetryLockPackageVersion = "1.1.0"
    , poetryLockPackageCategory = Just "main"
    , poetryLockPackageOptional = False
    , poetryLockPackageDependencies = Map.empty
    , poetryLockPackagePythonVersions = "*"
    , poetryLockPackageSource =
        Just
          ( PoetryLockPackageSource
              "file"
              "pkgFile-1.1.0.tar.gz"
              Nothing
              Nothing
          )
    }

expectedDeps :: [Dependency]
expectedDeps =
  [ dep PipType "flake8" (Just $ CCompatible "1.1") prodEnvs
  , dep GitType "https://github.com/pallets/flask.git" (Just $ CEq "38eb5d3b") prodEnvs
  , dep GitType "https://github.com/networkx/networkx.git" Nothing prodEnvs
  , dep GitType "https://github.com/numpy/numpy.git" (Just $ CEq "v0.13.2") prodEnvs
  , dep GitType "https://github.com/kennethreitz/requests.git" (Just $ CEq "next") prodEnvs
  , dep URLType "https://example.com/my-package-0.1.0.tar.gz" Nothing prodEnvs
  , dep PipType "black" (Just $ CEq "19.10b0") prodEnvs
  , dep PipType "pytest" Nothing devEnvs
  ]
  where
    dep :: DepType -> Text -> Maybe VerConstraint -> DepEnvironment -> Dependency
    dep t n v e = Dependency t n v [] (Set.singleton e) Map.empty
    prodEnvs :: DepEnvironment
    prodEnvs = EnvProduction
    devEnvs :: DepEnvironment
    devEnvs = EnvDevelopment

spec :: Spec
spec = do
  nominalContents <- runIO (TIO.readFile "test/Python/Poetry/testdata/pyproject1.toml")
  emptyContents <- runIO (TIO.readFile "test/Python/Poetry/testdata/pyproject2.toml")

  describe "toCanonicalName" $ do
    it "should convert text to lowercase" $
      toCanonicalName "GreatScore" `shouldBe` "greatscore"
    it "should replace underscore (_) to hyphens (-)" $
      toCanonicalName "my_oh_so_great_pkg" `shouldBe` "my-oh-so-great-pkg"

  describe "getDependencies" $
    it "should get all dependencies" $
      pyProjectDeps expectedPyProject `shouldMatchList` expectedDeps

  describe "supportedPyProjectDep" $
    it "should return false when dependency is sourced from local path" $
      supportedPyProjectDep notSupportedPyProjectDependency `shouldBe` False

  describe "supportedPoetryLockDep" $
    it "should return false when dependency is sourced from local path" $
      supportedPoetryLockDep notSupportedPoetryLockDependency `shouldBe` False

  describe "getPoetryBuildBackend" $ do
    describe "when provided with poetry build backend" $
      it "should return true" $
        getPoetryBuildBackend <$> (Toml.decode pyProjectCodec nominalContents)
          `shouldBe` Right (Just "poetry.core.masonry.api")

    describe "when not provided with any build system" $
      it "should return nothing" $
        getPoetryBuildBackend <$> Toml.decode pyProjectCodec emptyContents
          `shouldBe` Right Nothing

  describe "makePackageToLockDependencyMap" $ do
    it "should map poetry lock package to dependency" $
      makePackageToLockDependencyMap
        mempty
        [ PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgOne"
            , poetryLockPackageVersion = "1.21.0"
            , poetryLockPackageCategory = Just "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies =
                Map.fromList
                  [ ("pkgOneChildofOne", TextVersion "*")
                  , ("pkgTwoChildofOne", ObjectVersionSpec $ ObjectVersion "5.4")
                  , ("pkgThreeChildofOne", MultipleObjectVersionSpec [ObjectVersion ">=1.0,<2.0", ObjectVersion ">=1.6,<2.0"])
                  ]
            , poetryLockPackagePythonVersions = ">=3.7"
            , poetryLockPackageSource = Nothing
            }
        ]
        `shouldBe` Map.fromList
          [
            ( PackageName "pkgone"
            , Dependency
                { dependencyType = PipType
                , dependencyName = "pkgOne"
                , dependencyVersion = Just $ CEq "1.21.0"
                , dependencyLocations = []
                , dependencyEnvironments = Set.singleton EnvProduction
                , dependencyTags = Map.empty
                }
            )
          ]

    describe "when poetry lock dependency is from git source" $
      it "should replace poetry lock package name to git url" $
        makePackageToLockDependencyMap
          mempty
          [ PoetryLockPackage
              { poetryLockPackageName = PackageName "pkgWithGitSource"
              , poetryLockPackageVersion = "5.22.0.post0"
              , poetryLockPackageCategory = Just "main"
              , poetryLockPackageOptional = False
              , poetryLockPackageDependencies = Map.empty
              , poetryLockPackagePythonVersions = "*"
              , poetryLockPackageSource = Just (PoetryLockPackageSource "git" "https://github.com/someUser/pkgWithGitSource.git" (Just "v1.1.1") (Just "598ac"))
              }
          ]
          `shouldBe` Map.fromList
            [
              ( PackageName "pkgwithgitsource"
              , Dependency
                  { dependencyType = GitType
                  , dependencyName = "https://github.com/someUser/pkgWithGitSource.git"
                  , dependencyVersion = Just $ CEq "v1.1.1"
                  , dependencyLocations = []
                  , dependencyEnvironments = Set.singleton EnvProduction
                  , dependencyTags = Map.empty
                  }
              )
            ]

    describe "when poetry lock dependency is from url source" $
      it "should replace poetry lock package name to url" $
        makePackageToLockDependencyMap
          mempty
          [ PoetryLockPackage
              { poetryLockPackageName = PackageName "pkgSourcedFromUrl"
              , poetryLockPackageVersion = "3.92.1"
              , poetryLockPackageCategory = Just "main"
              , poetryLockPackageOptional = False
              , poetryLockPackageDependencies = Map.empty
              , poetryLockPackagePythonVersions = "*"
              , poetryLockPackageSource = Just (PoetryLockPackageSource "url" "https://some-url.com/some-dir/pkgThree-3.92.1.tar.gz" Nothing Nothing)
              }
          ]
          `shouldBe` Map.fromList
            [
              ( PackageName "pkgsourcedfromurl"
              , Dependency
                  { dependencyType = URLType
                  , dependencyName = "https://some-url.com/some-dir/pkgThree-3.92.1.tar.gz"
                  , dependencyVersion = Just $ CEq "3.92.1"
                  , dependencyLocations = []
                  , dependencyEnvironments = Set.singleton EnvProduction
                  , dependencyTags = Map.empty
                  }
              )
            ]

    describe "when poetry lock dependency is from file source" $
      it "should replace poetry lock package name to filepath" $
        makePackageToLockDependencyMap
          mempty
          [ PoetryLockPackage
              { poetryLockPackageName = PackageName "pkgSourcedFromFile"
              , poetryLockPackageVersion = "1.21.0"
              , poetryLockPackageCategory = Just "main"
              , poetryLockPackageOptional = False
              , poetryLockPackageDependencies = Map.empty
              , poetryLockPackagePythonVersions = "*"
              , poetryLockPackageSource = Just (PoetryLockPackageSource "file" "pkgTwo-1.21.0.tar.gz" Nothing Nothing)
              }
          ]
          `shouldBe` Map.empty

    describe "when poetry lock dependency is from secondary sources" $
      it "should include url into dependency location" $
        makePackageToLockDependencyMap
          mempty
          [ PoetryLockPackage
              { poetryLockPackageName = PackageName "myprivatepkg"
              , poetryLockPackageVersion = "0.0.1"
              , poetryLockPackageCategory = Just "main"
              , poetryLockPackageOptional = False
              , poetryLockPackageDependencies = Map.empty
              , poetryLockPackagePythonVersions = ">=3.6"
              , poetryLockPackageSource = Just (PoetryLockPackageSource "legacy" "https://gitlab.com/api/v4/projects/packages/pypi/simple" (Just "gitlab") Nothing)
              }
          ]
          `shouldBe` Map.fromList
            [
              ( PackageName "myprivatepkg"
              , Dependency
                  { dependencyType = PipType
                  , dependencyName = "myprivatepkg"
                  , dependencyVersion = Just $ CEq "0.0.1"
                  , dependencyLocations = ["https://gitlab.com/api/v4/projects/packages/pypi/simple"]
                  , dependencyEnvironments = Set.singleton EnvProduction
                  , dependencyTags = Map.empty
                  }
              )
            ]
