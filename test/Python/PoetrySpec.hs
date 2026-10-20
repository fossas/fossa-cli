{-# LANGUAGE TemplateHaskell #-}

module Python.PoetrySpec (
  spec,
) where

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
  findProjects,
  graphFromPyProjectAndLockFile,
 )
import Strategy.Python.Poetry.PoetryLock (
  PackageName (..),
  PoetryLock (..),
  PoetryLockDependencySpec (..),
  PoetryLockPackage (..),
  PoetryLockPackageSource (..),
  PoetryMetadata (..),
 )
import Strategy.Python.Poetry.PyProject (PoetryDependency (..), PyProject (..), PyProjectBuildSystem (..), PyProjectMetadata (..), PyProjectPoetry (..), PyProjectTool (..))
import Strategy.Python.Util (Req (NameReq))
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldMatchList)
import Types (DependencyResults (..), GraphBreadth (..))

newPoetryLock :: [PoetryLockPackage] -> PoetryLock
newPoetryLock pkgs = PoetryLock pkgs $ PoetryMetadata "some-version" "some-hash" "some-poetry-version"

candidatePyProject :: PyProject
candidatePyProject =
  PyProject
    { pyprojectBuildSystem = Just $ PyProjectBuildSystem{buildBackend = Just "poetry.core.masonry.api"}
    , pyprojectProject = Nothing
    , pyprojectTool =
        Just $
          PyProjectTool
            { pyprojectPdm = Nothing
            , pyprojectPoetry = Just $ PyProjectPoetry Nothing Nothing Nothing (Map.fromList ([("flow_pipes", PoetryTextVersion "^1.21")])) mempty Nothing
            }
    }

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

pep621SourceOnlySpec :: Spec
pep621SourceOnlySpec = do
  currDir <- runIO getCurrentDir
  describe "Poetry graph with source-only [tool.poetry.dependencies] entries" $ do
    let absSpecDir = currDir </> $(mkRelDir "test/Python/Poetry/testdata/pep621-source-only")
    let pyprojectFile = absSpecDir </> $(mkRelFile "pyproject.toml")
    let lockfile = absSpecDir </> $(mkRelFile "poetry.lock")

    let poetryProject = PoetryProject (ProjectDir absSpecDir) (PyProjectTomlFile pyprojectFile) (Just $ PoetryLockFile lockfile)
    it' "create expected graph" $ do
      let privateLocation d = d{dependencyLocations = ["https://pypi.example.com/simple"]}
          privateLib = privateLocation $ mkPipProdDep "private-lib@2.0.10"
          privateTool = privateLocation $ mkPipProdDep "private-tool@0.3.0"
          requests = mkPipProdDep "requests@2.32.3"
      graph <- dependencyGraph <$> analyze poetryProject
      expectDeps' [privateLib, privateTool, requests] graph
      expectDirect' [privateLib, privateTool, requests] graph
      expectEdges' [] graph

spec :: Spec
spec = do
  poetryV1_5OrGtSpec
  pep621SourceOnlySpec
  poetryLockMismatchSpec
  lockCategorySpec
  discoverySpec

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

-- Poetry 2.x projects declare their dependencies in the PEP 621 [project] table and need no [tool.poetry] table.
pep621OnlyPyProject :: PyProject
pep621OnlyPyProject =
  PyProject
    { pyprojectBuildSystem = Just $ PyProjectBuildSystem{buildBackend = Just "poetry.core.masonry.api"}
    , pyprojectProject = Just $ PyProjectMetadata{pyprojectDependencies = Just [NameReq "requests" Nothing Nothing Nothing], pyprojectOptionalDependencies = Nothing}
    , pyprojectTool = Nothing
    }

lockPackage :: Text -> Text -> Map.Map Text PoetryLockDependencySpec -> Maybe PoetryLockPackageSource -> PoetryLockPackage
lockPackage name version deps source =
  PoetryLockPackage
    { poetryLockPackageName = PackageName name
    , poetryLockPackageVersion = version
    , poetryLockPackageCategory = Nothing
    , poetryLockPackageOptional = False
    , poetryLockPackageDependencies = deps
    , poetryLockPackagePythonVersions = "*"
    , poetryLockPackageSource = source
    }

poetryLockMismatchSpec :: Spec
poetryLockMismatchSpec =
  describe "graphFromPyProjectAndLockFile" $ do
    -- Fails until src/Strategy/Python/Poetry.hs:graphFromPyProjectAndLockFile is fixed: isDirect is False whenever pyproject.toml has no [tool.poetry] table, so a PEP 621 [project].dependencies entry is never direct with a lockfile although pyProjectDeps makes it direct without one. See the PR description.
    it "marks PEP 621 [project] dependencies direct when there is no [tool.poetry] table" $ do
      let lock =
            newPoetryLock
              [ lockPackage "requests" "2.32.3" (Map.fromList [("certifi", TextVersion ">=2017.4.17")]) Nothing
              , lockPackage "certifi" "2024.2.2" Map.empty Nothing
              ]
          requests = Dependency PipType "requests" (Just $ CEq "2.32.3") [] (Set.singleton EnvProduction) Map.empty
      Graphing.directList (graphFromPyProjectAndLockFile pep621OnlyPyProject lock) `shouldBe` [requests]

    -- Fails until src/Strategy/Python/Poetry.hs:graphFromPyProjectAndLockFile is fixed: a lock package whose source type is "file" is left out of the lock dependency map and logged as ignored, yet it still reaches the graph as a versionless PyPI dependency. See the PR description.
    it "does not report a lock package sourced from a local file" $ do
      let lock = newPoetryLock [lockPackage "localpkg" "0.1.0" Map.empty (Just $ PoetryLockPackageSource "file" "dist/localpkg-0.1.0.tar.gz" Nothing Nothing)]
      Graphing.vertexList (graphFromPyProjectAndLockFile candidatePyProject lock) `shouldBe` []

    -- Fails until src/Strategy/Python/Poetry.hs:graphFromPyProjectAndLockFile is fixed: a lock package whose source type is "directory" is a path dependency, which the docs say is never reported, but it is emitted as a pip dependency named after its relative path. See the PR description.
    it "does not report a lock package sourced from a local directory" $ do
      let lock = newPoetryLock [lockPackage "my-package" "0.1.0" Map.empty (Just $ PoetryLockPackageSource "directory" "../my-package" Nothing Nothing)]
      Graphing.vertexList (graphFromPyProjectAndLockFile candidatePyProject lock) `shouldBe` []

lockCategorySpec :: Spec
lockCategorySpec =
  describe "graphFromPyProjectAndLockFile" $
    it "uses the categories of a pre-1.5 lockfile as the environments" $ do
      let lock =
            PoetryLock
              [ (lockPackage "prodpkg" "1.0.0" Map.empty Nothing){poetryLockPackageCategory = Just "main"}
              , (lockPackage "devpkg" "2.0.0" Map.empty Nothing){poetryLockPackageCategory = Just "dev"}
              , (lockPackage "testpkg" "3.0.0" Map.empty Nothing){poetryLockPackageCategory = Just "test"}
              , (lockPackage "optionalpkg" "4.0.0" Map.empty Nothing){poetryLockPackageOptional = True}
              ]
              (PoetryMetadata "1.1" "some-hash" "*")
          dep name version envs = Dependency PipType name (Just $ CEq version) [] envs Map.empty
      -- A pre-1.5 lockfile says which environment a package belongs to, so nothing is relabelled as development
      -- and an optional package without a category keeps an empty environment set.
      Graphing.vertexList (graphFromPyProjectAndLockFile candidatePyProject lock)
        `shouldMatchList` [ dep "prodpkg" "1.0.0" (Set.singleton EnvProduction)
                          , dep "devpkg" "2.0.0" (Set.singleton EnvDevelopment)
                          , dep "testpkg" "3.0.0" (Set.singleton EnvTesting)
                          , dep "optionalpkg" "4.0.0" mempty
                          ]

discoverySpec :: Spec
discoverySpec = do
  currDir <- runIO getCurrentDir
  let testdata = currDir </> $(mkRelDir "test/Python/Poetry/testdata")
      project dir = PoetryProject (ProjectDir dir) (PyProjectTomlFile (dir </> $(mkRelFile "pyproject.toml")))

  describe "findProjects" $ do
    it' "finds a poetry project together with its lockfile" $ do
      let dir = testdata </> $(mkRelDir "no-category")
      projects <- findProjects dir
      projects `shouldBe'` [project dir (Just (PoetryLockFile (dir </> $(mkRelFile "poetry.lock"))))]

    it' "finds a poetry project without a lockfile" $ do
      let dir = testdata </> $(mkRelDir "pep621")
      projects <- findProjects dir
      projects `shouldBe'` [project dir Nothing]

    it' "skips a project that uv manages" $ do
      projects <- findProjects (testdata </> $(mkRelDir "uv-managed"))
      projects `shouldBe'` []

    it' "skips a project whose build backend is not poetry" $ do
      projects <- findProjects (testdata </> $(mkRelDir "setuptools-backend"))
      projects `shouldBe'` []

  describe "analyze" $
    it' "reports the manifest's dependencies as a partial graph when there is no lockfile" $ do
      let dir = testdata </> $(mkRelDir "pep621")
          requests = Dependency PipType "requests" (Just $ CGreaterOrEq "2.28.0") [] (Set.singleton EnvProduction) Map.empty
          flask = Dependency PipType "flask" (Just $ CGreaterOrEq "2.0") [] (Set.singleton EnvProduction) Map.empty
          pytest = Dependency PipType "pytest" (Just $ CCompatible "6.0") [] (Set.singleton EnvDevelopment) Map.empty
      results <- analyze (project dir Nothing)
      dependencyGraphBreadth results `shouldBe'` Partial
      dependencyManifestFiles results `shouldBe'` [dir </> $(mkRelFile "pyproject.toml")]
      expectDeps' [requests, flask, pytest] (dependencyGraph results)
      expectDirect' [requests, flask, pytest] (dependencyGraph results)
      expectEdges' [] (dependencyGraph results)
