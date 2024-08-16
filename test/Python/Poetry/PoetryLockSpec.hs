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
            , poetryLockPackageCategory = Just "some_other_category"
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
            , poetryLockPackageCategory = Nothing
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
            , poetryLockPackageCategory = Nothing
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
            , poetryLockPackageCategory = Nothing
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgTwoChildofOne"
            , poetryLockPackageVersion = "5.4"
            , poetryLockPackageCategory = Nothing
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgThreeChildofOne"
            , poetryLockPackageVersion = "1.6.1"
            , poetryLockPackageCategory = Nothing
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName "myprivatepkg"
            , poetryLockPackageVersion = "0.0.1"
            , poetryLockPackageCategory = Nothing
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
  describe "decoding toml file" $
    it "should produce expected output" $
      Toml.decode contents
        `shouldBe` Toml.Success
          [ "5:1: unexpected key: description in package[0]"
          , "21:1: unexpected key: description in package[1]"
          , "34:1: unexpected key: description in package[2]"
          , "54:28: unexpected key: markers in package[3].dependencies.pkgThreeChildofOne[0]"
          , "55:28: unexpected key: markers in package[3].dependencies.pkgThreeChildofOne[1]"
          , "57:38: unexpected key: markers in package[3].dependencies.pkgTwoChildofOne"
          , "45:1: unexpected key: description in package[3]"
          , "62:1: unexpected key: description in package[4]"
          , "69:1: unexpected key: description in package[5]"
          , "76:1: unexpected key: description in package[6]"
          , "83:1: unexpected key: description in package[7]"
          ]
          expectedPoetryLock
