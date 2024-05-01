{-# LANGUAGE TemplateHaskell #-}

module Python.PoetrySpec (
  spec,
)
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, splitOn)
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import GraphUtil (expectDeps', expectDirect', expectEdges')
import Graphing (Graphing)
import Graphing qualified
import Path (mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Python.Poetry (
  PoetryLockFile (PoetryLockFile),
  PoetryProject (PoetryProject),
  ProjectDir (ProjectDir),
  PyProjectTomlFile (PyProjectTomlFile),
  analyze,
  graphFromPyProjectAndLockFile,
 )
import Strategy.Python.Poetry.PoetryLock (
  PackageName (..),
  PoetryLock (..),
  PoetryLockDependencySpec (..),
  PoetryLockPackage (..),
  PoetryMetadata (..),
 )
import Strategy.Python.Poetry.PyProject (PoetryDependency (..), PyProject (..), PyProjectBuildSystem (..), PyProjectPoetry (..))
import Test.Effect (it')
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Types (DependencyResults (dependencyGraph))

newPoetryLock :: [PoetryLockPackage] -> PoetryLock
newPoetryLock pkgs = PoetryLock pkgs $ PoetryMetadata "some-version" "some-hash" "some-poetry-version"

candidatePyProject :: PyProject
candidatePyProject =
  PyProject
    (Just $ PyProjectBuildSystem "poetry.core.masonry.api")
    (Just $ PyProjectPoetry Nothing Nothing Nothing (Map.fromList ([("flow_pipes", PoetryTextVersion "^1.21")])) mempty mempty mempty)
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
  poetryV1_5OrGtSpec

  describe "graphFromPyProjectAndLockFile" $ do
    it "should produce expected graph" $ do
      graphFromPyProjectAndLockFile candidatePyProject candidatePoetryLock `shouldBe` expectedGraph

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
        graphFromPyProjectAndLockFile candidatePyProject poetryLock `shouldBe` expectedGraphWithNoDeps

      describe "when package has deep dependencies" $ do
        it "should produce graph with edges" $ do
          let poetryLockDeps =
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
      -- >> poetry show -t --only "main, docs, dev, test"
      -- -
      -- >> click 8.1.7 Composable command line interface toolkit
      -- >> └── colorama *
      -- >> mkdocs 1.6.0 Project documentation with Markdown.
      -- >> ├── click >=7.0
      -- >> │   └── colorama *
      -- >> ├── colorama >=0.4
      -- >> ├── ghp-import >=1.0
      -- >> │   └── python-dateutil >=2.8.1
      -- >> │       └── six >=1.5
      -- >> ├── jinja2 >=2.11.1
      -- >> │   └── markupsafe >=2.0
      -- >> ├── markdown >=3.3.6
      -- >> ├── markupsafe >=2.0.1
      -- >> ├── mergedeep >=1.3.4
      -- >> ├── mkdocs-get-deps >=0.2.0
      -- >> │   ├── mergedeep >=1.3.4
      -- >> │   ├── platformdirs >=2.2.0
      -- >> │   └── pyyaml >=5.1
      -- >> ├── packaging >=20.5
      -- >> ├── pathspec >=0.11.1
      -- >> ├── pyyaml >=5.1
      -- >> ├── pyyaml-env-tag >=0.1
      -- >> │   └── pyyaml *
      -- >> └── watchdog >=2.0
      -- >> pytest 6.2.5 pytest: simple powerful testing with Python
      -- >> ├── atomicwrites >=1.0
      -- >> ├── attrs >=19.2.0
      -- >> ├── colorama *
      -- >> ├── iniconfig *
      -- >> ├── packaging *
      -- >> ├── pluggy >=0.12,<2.0
      -- >> ├── py >=1.8.2
      -- >> └── toml *
      -- >> pytest-mock 3.14.0 Thin-wrapper around the mock package for easier use with pytest
      -- >> └── pytest >=6.2.5
      -- >>     ├── atomicwrites >=1.0
      -- >>     ├── attrs >=19.2.0
      -- >>     ├── colorama *
      -- >>     ├── iniconfig *
      -- >>     ├── packaging *
      -- >>     ├── pluggy >=0.12,<2.0
      -- >>     ├── py >=1.8.2
      -- >>     └── toml *
      -- >> rich 13.7.1 Render rich text, tables, progress bars, syntax highlighting, markdown and more to the terminal
      -- >> ├── markdown-it-py >=2.2.0
      -- >> │   └── mdurl >=0.1,<1.0
      -- >> └── pygments >=2.13.0,<3.0.0
      -- -
      graph <- dependencyGraph <$> analyze poetryProject
      expectDeps'
        [ click
        , colorama
        , pytest
        , atomicwrites
        , attrs
        , iniconfig
        , packaging
        , pluggy
        , py
        , pytestMock
        , rich
        , markdownItPy
        , mdurl
        , pygments
        , ghpImport
        , jinja2
        , markdown
        , markupsafe
        , mergedeep
        , mkdocs
        , mkdocsGetDeps
        , pathspec
        , platformdirs
        , pythonDateUtil
        , pyyaml
        , pyyamlEnvTag
        , six
        , toml
        , watchdog
        ]
        graph
      expectEdges'
        [ (click, colorama)
        , (mkdocs, click)
        , (mkdocs, colorama)
        , (mkdocs, ghpImport)
        , (ghpImport, pythonDateUtil)
        , (pythonDateUtil, six)
        , (mkdocs, jinja2)
        , (jinja2, markupsafe)
        , (mkdocs, markdown)
        , (mkdocs, markupsafe)
        , (mkdocs, mergedeep)
        , (mkdocs, mkdocsGetDeps)
        , (mkdocsGetDeps, mergedeep)
        , (mkdocsGetDeps, platformdirs)
        , (mkdocsGetDeps, pyyaml)
        , (mkdocs, packaging)
        , (mkdocs, pathspec)
        , (mkdocs, pyyaml)
        , (mkdocs, pyyamlEnvTag)
        , (pyyamlEnvTag, pyyaml)
        , (mkdocs, watchdog)
        , (pytest, atomicwrites)
        , (pytest, attrs)
        , (pytest, colorama)
        , (pytest, iniconfig)
        , (pytest, packaging)
        , (pytest, pluggy)
        , (pytest, py)
        , (pytest, toml)
        , (pytestMock, pytest)
        , (rich, markdownItPy)
        , (markdownItPy, mdurl)
        , (rich, pygments)
        ]
        graph
      expectDirect' [pytest, pytestMock, rich, click] graph

click :: Dependency
click = mkPipDevDep "click@8.1.7"

colorama :: Dependency
colorama = mkPipDevDep "colorama@0.4.6"

pytest :: Dependency
pytest = mkPipDevDep "pytest@6.2.5"

atomicwrites :: Dependency
atomicwrites = mkPipDevDep "atomicwrites@1.4.1"

attrs :: Dependency
attrs = mkPipDevDep "attrs@23.2.0"

iniconfig :: Dependency
iniconfig = mkPipDevDep "iniconfig@2.0.0"

packaging :: Dependency
packaging = mkPipDevDep "packaging@24.0"

pluggy :: Dependency
pluggy = mkPipDevDep "pluggy@1.5.0"

py :: Dependency
py = mkPipDevDep "py@1.11.0"

pytestMock :: Dependency
pytestMock = mkPipDevDep "pytest-mock@3.14.0"

rich :: Dependency
rich = mkPipProdDep "rich@13.7.1"

markdownItPy :: Dependency
markdownItPy = mkPipProdDep "markdown-it-py@3.0.0"

mdurl :: Dependency
mdurl = mkPipProdDep "mdurl@0.1.2"

pygments :: Dependency
pygments = mkPipProdDep "pygments@2.17.2"

ghpImport :: Dependency
ghpImport = mkPipDevDep "ghp-import@2.1.0"

jinja2 :: Dependency
jinja2 = mkPipDevDep "jinja2@3.1.3"

markdown :: Dependency
markdown = mkPipDevDep "markdown@3.6"

markupsafe :: Dependency
markupsafe = mkPipDevDep "markupsafe@2.1.5"

mergedeep :: Dependency
mergedeep = mkPipDevDep "mergedeep@1.3.4"

mkdocs :: Dependency
mkdocs = mkPipDevDep "mkdocs@1.6.0"

mkdocsGetDeps :: Dependency
mkdocsGetDeps = mkPipDevDep "mkdocs-get-deps@0.2.0"

pathspec :: Dependency
pathspec = mkPipDevDep "pathspec@0.12.1"

platformdirs :: Dependency
platformdirs = mkPipDevDep "platformdirs@4.2.1"

pythonDateUtil :: Dependency
pythonDateUtil = mkPipDevDep "python-dateutil@2.9.0.post0"

pyyaml :: Dependency
pyyaml = mkPipDevDep "pyyaml@6.0.1"

pyyamlEnvTag :: Dependency
pyyamlEnvTag = mkPipDevDep "pyyaml-env-tag@0.1"

six :: Dependency
six = mkPipDevDep "six@1.16.0"

toml :: Dependency
toml = mkPipDevDep "toml@0.10.2"

watchdog :: Dependency
watchdog = mkPipDevDep "watchdog@4.0.0"

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
