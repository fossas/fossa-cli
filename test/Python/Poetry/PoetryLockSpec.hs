module Python.Poetry.PoetryLockSpec (
  spec,
) where

import Data.Map qualified as Map
import Data.Text.IO qualified as TIO
import Strategy.Python.Poetry.PoetryLock (
  ObjectVersion (..),
  PackageName (..),
  PoetryLock (..),
  PoetryLockDependencySpec (..),
  PoetryLockPackage (..),
  PoetryLockPackageSource (..),
  PoetryMetadata (..),
  poetryLockCodec,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  runIO,
  shouldBe,
 )
import Toml qualified

expectedPoetryLock :: PoetryLock
expectedPoetryLock =
  PoetryLock
    { poetryLockMetadata =
        PoetryMetadata
          "1.1"
          "cf14fd7e0a1a1c6c5a1ee9afe16d0abaaac531ab9d84ad3d1d5276634aa35687"
          "^3.8"
    , poetryLockPackages =
        [ PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgWithGitSource"
            , poetryLockPackageVersion = "5.22.0.post0"
            , poetryLockPackageCategory = Just "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource =
                Just
                  ( PoetryLockPackageSource
                      "git"
                      "https://github.com/someUser/pkgWithGitSource.git"
                      (Just "v1.1.1")
                      (Just "598ac")
                  )
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgSourcedFromFile"
            , poetryLockPackageVersion = "1.21.0"
            , poetryLockPackageCategory = Just "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource =
                Just
                  ( PoetryLockPackageSource
                      "file"
                      "pkgTwo-1.21.0.tar.gz"
                      Nothing
                      Nothing
                  )
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgSourcedFromUrl"
            , poetryLockPackageVersion = "3.92.1"
            , poetryLockPackageCategory = Just "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource =
                Just
                  ( PoetryLockPackageSource
                      "url"
                      "https://some-url.com/some-dir/pkgThree-3.92.1.tar.gz"
                      Nothing
                      Nothing
                  )
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgOne"
            , poetryLockPackageVersion = "1.21.0"
            , poetryLockPackageCategory = Just "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies =
                Map.fromList
                  [ ("pkgOneChildofOne", TextVersion "*")
                  , ("pkgTwoChildofOne", ObjectVersionSpec $ ObjectVersion "5.4")
                  ,
                    ( "pkgThreeChildofOne"
                    , MultipleObjectVersionSpec
                        [ ObjectVersion ">=1.0,<2.0"
                        , ObjectVersion ">=1.6,<2.0"
                        ]
                    )
                  ]
            , poetryLockPackagePythonVersions = ">=3.7"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgOneChildofOne"
            , poetryLockPackageVersion = "11.4"
            , poetryLockPackageCategory = Just "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgTwoChildofOne"
            , poetryLockPackageVersion = "5.4"
            , poetryLockPackageCategory = Just "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgThreeChildofOne"
            , poetryLockPackageVersion = "1.6.1"
            , poetryLockPackageCategory = Just "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "myprivatepkg"
            , poetryLockPackageVersion = "0.0.1"
            , poetryLockPackageCategory = Just "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = ">=3.6"
            , poetryLockPackageSource =
                Just
                  ( PoetryLockPackageSource
                      "legacy"
                      "https://gitlab.com/api/v4/projects/packages/pypi/simple"
                      (Just "gitlab")
                      Nothing
                  )
            }
        ]
    }

spec :: Spec
spec = do
  contents <- runIO (TIO.readFile "test/Python/Poetry/testdata/poetry.lock")
  describe "poetryLockCodec" $
    it "should produce expected output" $
      Toml.decode poetryLockCodec contents `shouldBe` Right expectedPoetryLock
