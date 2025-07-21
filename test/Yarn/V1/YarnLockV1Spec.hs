{-# LANGUAGE TemplateHaskell #-}

module Yarn.V1.YarnLockV1Spec (
  spec,
) where

import Control.Effect.Diagnostics
import Control.Effect.Lift
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (decodeUtf8, toString)
import Data.Tagged (applyTag)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
  insertEnvironment,
 )
import Effect.ReadFS (ReadFS, readContentsBS)
import GraphUtil (expectDeps', expectDirect', expectEdges')
import Debug.Trace (traceShow)
import Graphing qualified
import Path
import Path.IO (getCurrentDir)
import Strategy.Node.PackageJson (Development, FlatDeps (FlatDeps), NodePackage (NodePackage), Production)
import Strategy.Node.YarnV1.YarnLock (buildGraph, mangleParseErr)
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe)
import Yarn.Lock qualified as YL

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageOne"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

prodPackageOne :: Dependency
prodPackageOne = insertEnvironment EnvProduction packageOne

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageTwo"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

prodPackageTwo :: Dependency
prodPackageTwo = insertEnvironment EnvProduction packageTwo

packageThree :: Dependency
packageThree =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageThree"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageThree"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

prodDevPackageThree :: Dependency
prodDevPackageThree = insertEnvironment EnvProduction $ insertEnvironment EnvDevelopment packageThree

packageFour :: Dependency
packageFour =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageFour"
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageFour"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

packageFive :: Dependency
packageFive =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageFive"
    , dependencyVersion = Just (CEq "5.0.0")
    , dependencyLocations = ["https://someurl.io/somefile.gz"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

packageSix :: Dependency
packageSix =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageSix"
    , dependencyVersion = Just $ CEq "6.0.0"
    , dependencyLocations = ["https://registry.npmjs.org/packageSix"]
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

packageSeven :: Dependency
packageSeven =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageSeven"
    , dependencyVersion = Just $ CEq "7.0.0"
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

devPackageSix :: Dependency
devPackageSix = insertEnvironment EnvDevelopment packageSix

prodPackageSeven :: Dependency
prodPackageSeven = insertEnvironment EnvProduction packageSeven

packageOnce :: Dependency
packageOnce =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "once"
    , dependencyVersion = Just $ CEq "1.4.0"
    , dependencyLocations = ["https://registry.npmjs.org/once"]
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

prodPackageOnce :: Dependency
prodPackageOnce = insertEnvironment EnvProduction packageOnce

simpleFlatDeps :: FlatDeps
simpleFlatDeps =
  FlatDeps
    (applyTag @Production $ Set.fromList [NodePackage "packageOne" "^1.0.0", NodePackage "once" "^1.3.0", NodePackage "packageSeven" "^7.0.0"])
    (applyTag @Development $ Set.fromList [NodePackage "packageSix" "^6.0.0"])
    mempty
    mempty

parseFile :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Rel File -> m YL.Lockfile
parseFile name = do
  let testdataRoot = $(mkRelDir "test/Yarn/V1/testdata/")
  curDir <- sendIO getCurrentDir
  let testFilePath = (curDir </> testdataRoot </> name)
  testFile <- readContentsBS testFilePath
  let result = YL.parse (toString testFilePath) (decodeUtf8 testFile)
  tagError (mangleParseErr (toString testFilePath)) result

spec :: Spec
spec = do
  describe "buildGraph without package.json info" $ do
    it' "should produce expected structure" $ do
      yarnLock <- parseFile $(mkRelFile "yarn.lock")
      graph <- buildGraph yarnLock mempty
      expectDeps' [packageOne, packageTwo, packageThree, packageFour, packageFive, packageSix, packageSeven, packageOnce] graph
      expectDirect' [] graph
      expectEdges'
        [ (packageOne, packageTwo)
        , (packageTwo, packageThree)
        , (packageSix, packageThree)
        ]
        graph

  describe "buildGraph with promotions" $
    it' "Should apply the correct dep environments" $ do
      yarnLock <- parseFile $(mkRelFile "yarn.lock")
      graph <- buildGraph yarnLock simpleFlatDeps
      expectDeps' [prodPackageOne, prodPackageTwo, prodDevPackageThree, packageFour, packageFive, devPackageSix, prodPackageSeven, prodPackageOnce] graph
      expectDirect' [prodPackageOne, devPackageSix, prodPackageOnce, prodPackageSeven] graph
      expectEdges'
        [ (prodPackageOne, prodPackageTwo)
        , (prodPackageTwo, prodDevPackageThree)
        , (devPackageSix, prodDevPackageThree)
        ]
        graph

  describe "Yarn resolutions have precedence over yarn.lock collisions" $
    it' "Fails until resolutions are respected" $ do
      yarnLock <- parseFile $(mkRelFile "yarn.lock.override")
      let flatDeps =
            FlatDeps
              (applyTag @Production $ Set.fromList [NodePackage "unzipper" "^0.10.14"])
              mempty
              (Set.fromList [NodePackage "unzipper" "^0.12.3"])
              mempty
      graph <- buildGraph yarnLock flatDeps
      let allDeps = Graphing.toList graph
      traceShow allDeps $ do
        let hasOld = any (\d -> dependencyName d == "unzipper" && dependencyVersion d == Just (CEq "0.10.14")) allDeps
            hasNew = any (\d -> dependencyName d == "unzipper" && dependencyVersion d == Just (CEq "0.12.3")) allDeps
        hasOld `shouldBe'` False
        hasNew `shouldBe'` True
