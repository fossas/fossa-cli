{-# LANGUAGE TemplateHaskell #-}

module Python.PoetrySpec
  ( spec,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import Graphing (Graphing)
import Graphing qualified
import Path (mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Python.Poetry
  ( PoetryLockFile (PoetryLockFile),
    PoetryProject (PoetryProject),
    ProjectDir (ProjectDir),
    PyProjectTomlFile (PyProjectTomlFile),
    analyze,
    graphFromPyProjectAndLockFile,
    setGraphDirectsFromPyproject,
  )
import Strategy.Python.Poetry.PoetryLock
  ( PackageName (..),
    PoetryLock (..),
    PoetryLockDependencySpec (..),
    PoetryLockPackage (..),
    PoetryMetadata (..),
  )
import Strategy.Python.Poetry.PyProject (PoetryDependency (..), PyProject (..), PyProjectBuildSystem (..), PyProjectPoetry (..))
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Types (DependencyResults (dependencyGraph))
import Data.Text (Text, splitOn)
import GraphUtil ( expectDeps', expectEdges', expectDirect' )
import Text.Pretty.Simple (pShow, pShowNoColor)
import Control.Carrier.Lift (sendIO)
import Effect.Logger (logInfo)
import Effect.Logger (Pretty(..), logStdout)
import Data.Text.Lazy (toStrict)

newPoetryLock :: [PoetryLockPackage] -> PoetryLock
newPoetryLock pkgs = PoetryLock pkgs $ PoetryMetadata "some-version" "some-hash" "some-poetry-version"

candidatePyProject :: PyProject
candidatePyProject =
  PyProject
    (Just $ PyProjectBuildSystem "poetry.core.masonry.api")
    (Just $ PyProjectPoetry Nothing Nothing Nothing (Map.fromList ([("flow_pipes", PoetryTextVersion "^1.21")])) Map.empty Map.empty Map.empty)
    Nothing
    Nothing

candidatePoetryLock :: PoetryLock
candidatePoetryLock =
  newPoetryLock
    [ PoetryLockPackage
        { poetryLockPackageName = PackageName "flow_pipes",
          poetryLockPackageVersion = "1.21.0",
          poetryLockPackageCategory = Just "main",
          poetryLockPackageOptional = False,
          poetryLockPackageDependencies = Map.fromList [("flow_pipes_gravity", TextVersion "^1.1")],
          poetryLockPackagePythonVersions = "*",
          poetryLockPackageSource = Nothing
        },
      PoetryLockPackage
        { poetryLockPackageName = PackageName "flow_pipes_gravity",
          poetryLockPackageVersion = "1.1.1",
          poetryLockPackageCategory = Just "main",
          poetryLockPackageOptional = False,
          poetryLockPackageDependencies = Map.empty,
          poetryLockPackagePythonVersions = "*",
          poetryLockPackageSource = Nothing
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
  poetryV1_5OrGtSpec

  describe "setGraphDirectsFromPyproject" $
    it "should should promote direct dependencies and create valid graph" $
      (setGraphDirectsFromPyproject (graphFromPyProjectAndLockFile candidatePyProject candidatePoetryLock) candidatePyProject) `shouldBe` expectedGraph
  describe "graphFromLockFile" $ do
    describe "when package has no transitive dependencies" $ do
      it "should produce graph with no edges" $ do
        let poetryLock =
              newPoetryLock
                [ PoetryLockPackage
                    { poetryLockPackageName = PackageName "somePkg",
                      poetryLockPackageVersion = "1.21.0",
                      poetryLockPackageCategory = Just "main",
                      poetryLockPackageOptional = False,
                      poetryLockPackageDependencies = Map.empty,
                      poetryLockPackagePythonVersions = "*",
                      poetryLockPackageSource = Nothing
                    }
                ]
        graphFromPyProjectAndLockFile candidatePyProject poetryLock `shouldBe` expectedGraphWithNoDeps

      describe "when package has deep dependencies" $ do
        it "should produce graph with edges" $ do
          let poetryLockDeps =
                newPoetryLock
                  [ PoetryLockPackage
                      { poetryLockPackageName = PackageName "somePkg",
                        poetryLockPackageVersion = "1.21.0",
                        poetryLockPackageCategory = Just "main",
                        poetryLockPackageOptional = False,
                        poetryLockPackageDependencies = Map.fromList [("pkgOneChildOne", TextVersion "*")],
                        poetryLockPackagePythonVersions = "*",
                        poetryLockPackageSource = Nothing
                      },
                    PoetryLockPackage
                      { poetryLockPackageName = PackageName "pkgOneChildOne",
                        poetryLockPackageVersion = "1.22.0",
                        poetryLockPackageCategory = Just "main",
                        poetryLockPackageOptional = False,
                        poetryLockPackageDependencies = Map.empty,
                        poetryLockPackagePythonVersions = "*",
                        poetryLockPackageSource = Nothing
                      }
                  ]
          graphFromPyProjectAndLockFile candidatePyProject poetryLockDeps `shouldBe` expectedGraphWithDeps

poetryV1_5OrGtSpec :: Spec
poetryV1_5OrGtSpec = do
  currDir <- runIO getCurrentDir
  describe "Poetry graph" $ do
    let absSpecDir = currDir </> $(mkRelDir "test/Python/Poetry/testdata/no-category")
    let pyprojectFile = absSpecDir </> $(mkRelFile "pyproject.toml")
    let lockfile = absSpecDir </> $(mkRelFile "poetry.lock")

    let poetryProject = PoetryProject (ProjectDir absSpecDir) (PyProjectTomlFile pyprojectFile) (Just $ PoetryLockFile lockfile)
    it' "create expected graph" $ do
      -- -
      -- >> poetry show -t
      -- 
      -- click 8.1.7 Composable command line interface toolkit
      -- └── colorama *
      -- pytest 6.2.5 pytest: simple powerful testing with Python
      -- ├── atomicwrites >=1.0
      -- ├── attrs >=19.2.0
      -- ├── colorama *
      -- ├── iniconfig *
      -- ├── packaging *
      -- ├── pluggy >=0.12,<2.0
      -- ├── py >=1.8.2
      -- └── toml *
      -- pytest-mock 3.14.0 Thin-wrapper around the mock package for easier use with pytest
      -- └── pytest >=6.2.5
      --     ├── atomicwrites >=1.0 
      --     ├── attrs >=19.2.0 
      --     ├── colorama * 
      --     ├── iniconfig * 
      --     ├── packaging * 
      --     ├── pluggy >=0.12,<2.0 
      --     ├── py >=1.8.2 
      --     └── toml * 
      -- rich 13.7.1 Render rich text, tables, progress bars, syntax highlighting, markdown and more to the terminal
      -- ├── markdown-it-py >=2.2.0
      -- │   └── mdurl >=0.1,<1.0 
      -- └── pygments >=2.13.0,<3.0.0
      graph <- dependencyGraph <$> analyze poetryProject
      logStdout $ toStrict $ pShow graph
      expectDeps' deps graph
      expectDirect' deps graph
      expectEdges' [] graph

deps :: [Dependency]
deps = [click]

click :: Dependency
click = mkPipProdDep "click@8.1.7"

mkDep :: DepType -> Text -> Maybe DepEnvironment -> Dependency
mkDep dt nameAtVersion env = do
  let nameAndVersionSplit = splitOn "@" nameAtVersion
      name = head nameAndVersionSplit
      version = last nameAndVersionSplit
  Dependency
    dt
    name
    (CEq <$> (Just version))
    mempty
    (maybe mempty Set.singleton env)
    mempty

mkPipProdDep :: Text -> Dependency
mkPipProdDep nameAtVersion = mkDep PipType nameAtVersion (Just EnvProduction)

mkPipDevDep :: Text -> Dependency
mkPipDevDep nameAtVersion = mkDep PipType nameAtVersion (Just EnvDevelopment)
