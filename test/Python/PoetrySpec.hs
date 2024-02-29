module Python.PoetrySpec (
  spec,
) where

import Data.Map qualified as Map
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import Graphing (Graphing)
import Graphing qualified
import Strategy.Python.Poetry (graphFromLockFile, setGraphDirectsFromPyproject)
import Strategy.Python.Poetry.PoetryLock (
  PackageName (..),
  PoetryLock (..),
  PoetryLockDependencySpec (..),
  PoetryLockPackage (..),
  PoetryMetadata (..),
 )

import Data.Set qualified as Set
import Strategy.Python.Poetry.PyProject (PoetryDependency (..), PyProject (..), PyProjectBuildSystem (..), PyProjectPoetry (..))
import Test.Hspec

newPoetryLock :: [PoetryLockPackage] -> PoetryLock
newPoetryLock pkgs = PoetryLock pkgs $ PoetryMetadata "some-version" "some-hash" "some-poetry-version"

candidatePyProject :: PyProject
candidatePyProject =
  PyProject
    (Just $ PyProjectBuildSystem "poetry.core.masonry.api")
    (Just $ PyProjectPoetry Nothing Nothing Nothing (Map.fromList ([("flow_pipes", PoetryTextVersion "^1.21")])) Map.empty Map.empty)
    Nothing
    Nothing

candidatePoetryLock :: PoetryLock
candidatePoetryLock =
  newPoetryLock
    [ PoetryLockPackage
        { poetryLockPackageName = PackageName "flow_pipes"
        , poetryLockPackageVersion = "1.21.0"
        , poetryLockPackageCategory = Just "main"
        , poetryLockPackageOptional = False
        , poetryLockPackageDependencies = Map.fromList [("flow_pipes_gravity", TextVersion "^1.1")]
        , poetryLockPackagePythonVersions = "*"
        , poetryLockPackageSource = Nothing
        }
    , PoetryLockPackage
        { poetryLockPackageName = PackageName "flow_pipes_gravity"
        , poetryLockPackageVersion = "1.1.1"
        , poetryLockPackageCategory = Just "main"
        , poetryLockPackageOptional = False
        , poetryLockPackageDependencies = Map.empty
        , poetryLockPackagePythonVersions = "*"
        , poetryLockPackageSource = Nothing
        }
    ]

expectedGraph :: Graphing Dependency
expectedGraph =
  Graphing.edge
    (Dependency PipType "flow_pipes" (Just $ CEq "1.21.0") [] (Set.singleton EnvProduction) Map.empty)
    (Dependency PipType "flow_pipes_gravity" (Just $ CEq "1.1.1") [] (Set.singleton EnvProduction) Map.empty)
    <> Graphing.direct (Dependency PipType "flow_pipes" (Just $ CEq "1.21.0") [] (Set.singleton EnvProduction) Map.empty)

expectedGraphWithNoDeps :: Graphing Dependency
expectedGraphWithNoDeps = Graphing.deep (Dependency PipType "somePkg" (Just $ CEq "1.21.0") [] (Set.singleton EnvProduction) Map.empty)

expectedGraphWithDeps :: Graphing Dependency
expectedGraphWithDeps =
  Graphing.edge
    (Dependency PipType "somePkg" (Just $ CEq "1.21.0") [] (Set.singleton EnvProduction) Map.empty)
    (Dependency PipType "pkgOneChildOne" (Just $ CEq "1.22.0") [] (Set.singleton EnvProduction) Map.empty)

spec :: Spec
spec = do
  describe "setGraphDirectsFromPyproject" $
    it "should should promote direct dependencies and create valid graph" $
      setGraphDirectsFromPyproject (graphFromLockFile candidatePoetryLock) candidatePyProject `shouldBe` expectedGraph

  describe "graphFromLockFile" $ do
    describe "when package has no transitive dependencies" $ do
      it "should produce graph with no edges" $ do
        let poetryLock =
              newPoetryLock
                [ PoetryLockPackage
                    { poetryLockPackageName = PackageName "somePkg"
                    , poetryLockPackageVersion = "1.21.0"
                    , poetryLockPackageCategory = Just "main"
                    , poetryLockPackageOptional = False
                    , poetryLockPackageDependencies = Map.empty
                    , poetryLockPackagePythonVersions = "*"
                    , poetryLockPackageSource = Nothing
                    }
                ]
        graphFromLockFile poetryLock `shouldBe` expectedGraphWithNoDeps

      describe "when package has deep dependencies" $ do
        it "should produce graph with edges" $ do
          let peortyLockDeps =
                newPoetryLock
                  [ PoetryLockPackage
                      { poetryLockPackageName = PackageName "somePkg"
                      , poetryLockPackageVersion = "1.21.0"
                      , poetryLockPackageCategory = Just "main"
                      , poetryLockPackageOptional = False
                      , poetryLockPackageDependencies = Map.fromList [("pkgOneChildOne", TextVersion "*")]
                      , poetryLockPackagePythonVersions = "*"
                      , poetryLockPackageSource = Nothing
                      }
                  , PoetryLockPackage
                      { poetryLockPackageName = PackageName "pkgOneChildOne"
                      , poetryLockPackageVersion = "1.22.0"
                      , poetryLockPackageCategory = Just "main"
                      , poetryLockPackageOptional = False
                      , poetryLockPackageDependencies = Map.empty
                      , poetryLockPackagePythonVersions = "*"
                      , poetryLockPackageSource = Nothing
                      }
                  ]
          graphFromLockFile peortyLockDeps `shouldBe` expectedGraphWithDeps
