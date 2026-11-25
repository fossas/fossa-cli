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
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
  insertEnvironment,
 )
import Effect.ReadFS (ReadFS, readContentsBS)
import GraphUtil (expectDeps', expectDirect', expectEdges')
import Path
import Path.IO (getCurrentDir)
import Strategy.Node.PackageJson (Development, FlatDeps (FlatDeps), NodePackage (NodePackage), Production)
import Strategy.Node.YarnV1.YarnLock (buildGraph, mangleParseErr, parseLockfile)
import Test.Effect (it')
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

-- | A registry package with its @resolved@ URL as its location.
dep :: Text -> Text -> [Text] -> Dependency
dep name version locations =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = name
    , dependencyVersion = Just (CEq version)
    , dependencyLocations = locations
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

prod :: Dependency -> Dependency
prod = insertEnvironment EnvProduction

-- | @package.json@ installing @string-width@ under the alias @string-width-cjs@.
aliasFlatDeps :: FlatDeps
aliasFlatDeps = FlatDeps (applyTag @Production $ Set.singleton $ NodePackage "string-width-cjs" "npm:string-width@^4.2.0") mempty mempty

remotesFlatDeps :: FlatDeps
remotesFlatDeps =
  FlatDeps
    (applyTag @Production $ Set.fromList [NodePackage "@scope/pkg" "^1.1.0", NodePackage "foo" "^1.0.0"])
    (applyTag @Development $ Set.singleton $ NodePackage "localdep" "file:../localdep")
    mempty

parseFile :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Rel File -> m YL.Lockfile
parseFile name = do
  let testdataRoot = $(mkRelDir "test/Yarn/V1/testdata/")
  curDir <- sendIO getCurrentDir
  let testFilePath = (curDir </> testdataRoot </> name)
  testFile <- readContentsBS testFilePath
  let result = parseLockfile (toString testFilePath) (decodeUtf8 testFile)
  tagError (mangleParseErr (toString testFilePath)) result

spec :: Spec
spec = do
  describe "parseLockfile" $
    it' "should accept a lockfile with no package entries" $ do
      -- What `yarn install` writes for a project without dependencies: only the header comments.
      yarnLock <- parseFile $(mkRelFile "no-packages.yarn.lock")
      graph <- buildGraph yarnLock simpleFlatDeps
      expectDeps' [] graph
      expectDirect' [] graph

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

  describe "buildGraph with npm aliases" $
    it' "reports an alias under the aliased package's name" $ do
      yarnLock <- parseFile $(mkRelFile "alias.yarn.lock")
      graph <- buildGraph yarnLock aliasFlatDeps
      let cliui = dep "@isaacs/cliui" "8.0.2" ["https://registry.npmjs.org/@isaacs/cliui/-/cliui-8.0.2.tgz"]
          stringWidth = prod $ dep "string-width" "4.2.3" ["https://registry.npmjs.org/string-width/-/string-width-4.2.3.tgz"]
          wrapAnsi = dep "wrap-ansi" "7.0.0" ["https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-7.0.0.tgz"]
          scopedReal = dep "@scope/real" "1.0.0" ["https://registry.npmjs.org/@scope/real/-/real-1.0.0.tgz"]
      expectDeps' [cliui, stringWidth, wrapAnsi, scopedReal] graph
      expectDirect' [stringWidth] graph
      expectEdges' [(cliui, stringWidth), (wrapAnsi, stringWidth)] graph

  describe "buildGraph with scoped keys, duplicate versions and remotes" $
    it' "keeps duplicate versions apart and records git, file and link locations" $ do
      yarnLock <- parseFile $(mkRelFile "remotes.yarn.lock")
      graph <- buildGraph yarnLock remotesFlatDeps
      let scopedPkg = prod $ dep "@scope/pkg" "1.1.0" ["https://registry.npmjs.org/@scope/pkg/-/pkg-1.1.0.tgz"]
          fooOne = prod $ dep "foo" "1.2.0" ["https://registry.npmjs.org/foo/-/foo-1.2.0.tgz"]
          fooTwo = prod $ dep "foo" "2.1.0" ["https://registry.npmjs.org/foo/-/foo-2.1.0.tgz"]
          gitDep = prod $ dep "gitdep" "1.0.0" ["https://github.com/a/b.git@abc123"]
          localDep = insertEnvironment EnvDevelopment $ dep "localdep" "0.1.0" ["../localdep"]
          linkDep = dep "linkdep" "0.0.0" ["../linkdep"]
      expectDeps' [scopedPkg, fooOne, fooTwo, gitDep, localDep, linkDep] graph
      expectDirect' [scopedPkg, fooOne, localDep] graph
      expectEdges' [(scopedPkg, fooTwo), (scopedPkg, gitDep)] graph
