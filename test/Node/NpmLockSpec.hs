{-# LANGUAGE TemplateHaskell #-}

module Node.NpmLockSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (readContentsJson)
import GraphUtil (
  expectDeps,
  expectDeps',
  expectDirect,
  expectDirect',
  expectEdges,
  expectEdges',
 )
import Path (mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node.Npm.PackageLock (
  NpmDep (
    NpmDep,
    depDependencies,
    depDev,
    depRequires,
    depResolved,
    depVersion
  ),
  NpmPackageJson (..),
  NpmResolved (NpmResolved, unNpmResolved),
  buildGraph,
 )
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, it, runIO)

mockInput :: NpmPackageJson
mockInput =
  NpmPackageJson
    { packageDependencies =
        Map.fromList
          [
            ( "packageOne"
            , NpmDep
                { depVersion = "1.0.0"
                , depDev = False
                , depResolved = NpmResolved $ Just "https://example.com/one.tgz"
                , depRequires = Map.fromList [("packageTwo", "2.0.0"), ("packageSeven", "7.0.0")]
                , depDependencies =
                    Map.fromList
                      [
                        ( "packageTwo"
                        , NpmDep
                            { depVersion = "2.0.0"
                            , depDev = True
                            , depResolved = NpmResolved $ Just "https://example.com/two.tgz"
                            , depRequires = Map.fromList [("packageThree", "3.0.0")]
                            , depDependencies = mempty
                            }
                        )
                      ]
                }
            )
          ,
            ( "packageThree"
            , NpmDep
                { depVersion = "3.0.0"
                , depDev = True
                , depResolved = NpmResolved $ Just "https://example.com/three.tgz"
                , depRequires = Map.fromList [("packageOne", "1.0.0")]
                , depDependencies = mempty
                }
            )
          ,
            ( "packageSeven"
            , NpmDep
                { depVersion = "7.0.0"
                , depDev = False
                , depResolved = NpmResolved $ Just "https://example.com/seven.tgz"
                , depRequires = mempty
                , depDependencies = mempty
                }
            )
          ,
            ( "packageFour"
            , NpmDep
                { depVersion = "file:abc/def"
                , depDev = False
                , depResolved = NpmResolved Nothing
                , depRequires = mempty
                , depDependencies = mempty
                }
            )
          ,
            ( "packageFive"
            , NpmDep
                { depVersion = "5.0.0"
                , depDev = True
                , depResolved = NpmResolved $ Just "https://example.com/five.tgz"
                , depRequires = Map.fromList [("packageSix", "6.0.0")]
                , depDependencies = mempty
                }
            )
          ,
            ( "packageSix"
            , NpmDep
                { depVersion = "6.0.0"
                , depDev = True
                , depResolved = NpmResolved $ Just "https://example.com/six.tgz"
                , depRequires = mempty
                , depDependencies = mempty
                }
            )
          ]
    }

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = ["https://example.com/one.tgz"]
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = ["https://example.com/two.tgz"]
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

packageThree :: Dependency
packageThree =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageThree"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = ["https://example.com/three.tgz"]
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

packageFive :: Dependency
packageFive =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageFive"
    , dependencyVersion = Just (CEq "5.0.0")
    , dependencyLocations = ["https://example.com/five.tgz"]
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

packageSix :: Dependency
packageSix =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageSix"
    , dependencyVersion = Just (CEq "6.0.0")
    , dependencyLocations = ["https://example.com/six.tgz"]
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

packageSeven :: Dependency
packageSeven =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageSeven"
    , dependencyVersion = Just (CEq "7.0.0")
    , dependencyLocations = ["https://example.com/seven.tgz"]
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

argparseDirect :: Dependency
argparseDirect =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "argparse"
    , dependencyVersion = Just $ CEq "1.0.10"
    , dependencyLocations = ["https://registry.npmjs.org/argparse/-/argparse-1.0.10.tgz"]
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = mempty
    }

argparseDeep :: Dependency
argparseDeep =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "argparse"
    , dependencyVersion = Just $ CEq "2.0.1"
    , dependencyLocations = ["https://registry.npmjs.org/argparse/-/argparse-2.0.1.tgz"]
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = mempty
    }

jsyaml :: Dependency
jsyaml =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "js-yaml"
    , dependencyVersion = Just $ CEq "4.1.0"
    , dependencyLocations = ["https://registry.npmjs.org/js-yaml/-/js-yaml-4.1.0.tgz"]
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = mempty
    }

sprintf :: Dependency
sprintf =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "sprintf-js"
    , dependencyVersion = Just $ CEq "1.0.3"
    , dependencyLocations = ["https://registry.npmjs.org/sprintf-js/-/sprintf-js-1.0.3.tgz"]
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = mempty
    }

spec :: Spec
spec = do
  curdir <- runIO getCurrentDir
  let testDir = curdir </> $(mkRelDir "test/Node/testdata")

  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mockInput (Set.fromList ["packageOne", "packageThree", "packageFive"])
      expectDeps [packageOne, packageTwo, packageThree, packageFive, packageSix, packageSeven] graph
      expectDirect [packageOne, packageThree, packageFive] graph
      expectEdges
        [ (packageOne, packageTwo)
        , (packageOne, packageSeven)
        , (packageTwo, packageThree)
        , (packageThree, packageOne)
        , (packageFive, packageSix)
        ]
        graph

    it' "should process nested dependencies" $ do
      parsed <- readContentsJson (testDir </> $(mkRelFile "nested-deps.json"))
      let graph = buildGraph parsed (Set.fromList ["argparse", "js-yaml", "sprintf-js"])
      expectDeps' [argparseDeep, argparseDirect, jsyaml, sprintf] graph
      expectDirect' [argparseDirect, jsyaml, sprintf] graph
      expectEdges'
        [ (jsyaml, argparseDeep)
        , (argparseDirect, sprintf)
        ]
        graph

  describe "parsing package-json.lock" $ do
    it' "Should ignore \"resolved\": <bool> in package-lock.json" $ do
      let packageLock = testDir </> $(mkRelFile "boolean-resolved-package-lock.json")
      NpmPackageJson{packageDependencies = packageDependencies} <- readContentsJson packageLock
      let foo = unNpmResolved . depResolved =<< Map.lookup "foo" packageDependencies
      foo `shouldBe'` Nothing

    it' "Should parse \"resolved\": <string> in package-lock.json" $ do
      let packageLock = testDir </> $(mkRelFile "string-resolved-package-lock.json")
      NpmPackageJson{packageDependencies = packageDependencies} <- readContentsJson packageLock
      let foo = unNpmResolved . depResolved =<< Map.lookup "foo" packageDependencies
      foo `shouldBe'` Just "https://bar.npmjs.org/foo/-/foo-1.0.0.tgz"

    it' "Should parse dependency with no \"resolved\" key in package-lock.json" $ do
      let packageLock = testDir </> $(mkRelFile "absent-resolved-package-lock.json")
      NpmPackageJson{packageDependencies = packageDependencies} <- readContentsJson packageLock
      let foo = unNpmResolved . depResolved =<< Map.lookup "foo" packageDependencies
      foo `shouldBe'` Nothing
