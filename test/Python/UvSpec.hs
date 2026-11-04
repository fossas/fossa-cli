{-# LANGUAGE QuasiQuotes #-}

module Python.UvSpec (
  spec,
) where

import App.Fossa.Config.ConfigFile (UvConfig (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import Effect.ReadFS (readContentsToml)
import GraphUtil
import Graphing (Graphing, directList, vertexList)
import Path (relfile)
import Path.IO (makeAbsolute)
import Strategy.Python.Uv
import Test.Effect
import Test.Hspec

--              my-project
--     /    /       \           \
--   dep1  dep2   dep3 (dev)  dep4 (dev)
--    |             |            |
--   dep4          dep6         dep5
--    |
--   dep5
lock :: UvLock
lock =
  UvLock
    { uvlockPackages =
        [ UvLockPackage
            { uvlockPackageName = "my-project"
            , uvlockPackageVersion = Just "0.1.0"
            , uvlockPackageSource = SourceVirtual "."
            , uvlockPackageDependencies = map mkPackageDep ["dep1", "dep2"]
            , uvlockPackageDevDependencies = Map.fromList [("dev", map mkPackageDep ["dep3", "dep4"])]
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = Just "1.1.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = [mkPackageDep "dep4"]
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep2"
            , uvlockPackageVersion = Just "2.1.2"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep3"
            , uvlockPackageVersion = Just "0.9.9"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = [mkPackageDep "dep6"]
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep4"
            , uvlockPackageVersion = Just "1.0.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = [mkPackageDep "dep5"]
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep5"
            , uvlockPackageVersion = Just "3.0.1"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep6"
            , uvlockPackageVersion = Just "1.1.1"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        ]
    }

--     my-project
--     /    \
--   dep1  dep3 (dev)
--           |
--         dep6
lockNewStyleDevDeps :: UvLock
lockNewStyleDevDeps =
  UvLock
    { uvlockPackages =
        [ UvLockPackage
            { uvlockPackageName = "my-project"
            , uvlockPackageVersion = Just "0.1.0"
            , uvlockPackageSource = SourceVirtual "."
            , uvlockPackageDependencies = map mkPackageDep ["dep1", "dep2"]
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = Map.fromList [("dev", [mkPackageDep "dep3"])]
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = Just "1.1.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep3"
            , uvlockPackageVersion = Just "0.9.9"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = [mkPackageDep "dep6"]
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep6"
            , uvlockPackageVersion = Just "1.1.1"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        ]
    }

--     my-project (editable, no version -- dynamic version project)
--     /    \
--   dep1  dep3 (dev)
--           |
--         dep6
lockEditableNoVersion :: UvLock
lockEditableNoVersion =
  UvLock
    { uvlockPackages =
        [ UvLockPackage
            { uvlockPackageName = "my-project"
            , uvlockPackageVersion = Nothing
            , uvlockPackageSource = SourceEditable "."
            , uvlockPackageDependencies = [mkPackageDep "dep1"]
            , uvlockPackageDevDependencies = Map.fromList [("dev", [mkPackageDep "dep3"])]
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = Just "1.1.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep3"
            , uvlockPackageVersion = Just "0.9.9"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = [mkPackageDep "dep6"]
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep6"
            , uvlockPackageVersion = Just "1.1.1"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        ]
    }

--     my-project
--     /    \
--   dep1  local-lib (directory dep)
lockWithDirectory :: UvLock
lockWithDirectory =
  UvLock
    { uvlockPackages =
        [ UvLockPackage
            { uvlockPackageName = "my-project"
            , uvlockPackageVersion = Just "0.1.0"
            , uvlockPackageSource = SourceVirtual "."
            , uvlockPackageDependencies = map mkPackageDep ["dep1", "local-lib"]
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = Just "1.1.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "local-lib"
            , uvlockPackageVersion = Just "0.2.0"
            , uvlockPackageSource = SourceDirectory "../local-lib"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        ]
    }

--       my-project
--      /          \
--   foo 1.0    foo 2.0 (dev)
--                  |
--                 bar
lockSplitVersions :: UvLock
lockSplitVersions =
  UvLock
    { uvlockPackages =
        [ UvLockPackage
            { uvlockPackageName = "my-project"
            , uvlockPackageVersion = Just "0.1.0"
            , uvlockPackageSource = SourceVirtual "."
            , uvlockPackageDependencies = [UvLockPackageDependency "foo" (Just "1.0.0") Nothing]
            , uvlockPackageDevDependencies = Map.fromList [("dev", [UvLockPackageDependency "foo" (Just "2.0.0") Nothing])]
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "foo"
            , uvlockPackageVersion = Just "1.0.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "foo"
            , uvlockPackageVersion = Just "2.0.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = [mkPackageDep "bar"]
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "bar"
            , uvlockPackageVersion = Just "1.0.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = mempty
            , uvlockPackageOptionalDependencies = mempty
            }
        ]
    }

mkPackageDep :: Text -> UvLockPackageDependency
mkPackageDep name = UvLockPackageDependency name Nothing Nothing

mkDep :: Text -> Text -> [DepEnvironment] -> Dependency
mkDep name version envs =
  Dependency
    { dependencyType = PipType
    , dependencyName = name
    , dependencyVersion = Just (CEq version)
    , dependencyLocations = []
    , dependencyEnvironments = Set.fromList envs
    , dependencyTags = mempty
    }

-- Each package in the graph, by name, with its environments.
environments :: Graphing Dependency -> Map Text (Set DepEnvironment)
environments = Map.fromList . map (\dep -> (dependencyName dep, dependencyEnvironments dep)) . vertexList

dep1 :: Dependency
dep1 = mkDep "dep1" "1.1.0" [EnvProduction]

dep2 :: Dependency
dep2 = mkDep "dep2" "2.1.2" [EnvProduction]

dep3 :: Dependency
dep3 = mkDep "dep3" "0.9.9" [EnvDevelopment]

dep4 :: Dependency
dep4 = mkDep "dep4" "1.0.0" [EnvProduction, EnvDevelopment]

dep5 :: Dependency
dep5 = mkDep "dep5" "3.0.1" [EnvProduction, EnvDevelopment]

dep6 :: Dependency
dep6 = mkDep "dep6" "1.1.1" [EnvDevelopment]

localLib :: Dependency
localLib =
  Dependency
    { dependencyType = UnresolvedPathType
    , dependencyName = "../local-lib"
    , dependencyVersion = Just (CEq "0.2.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.fromList [EnvProduction]
    , dependencyTags = mempty
    }

anyio :: Dependency
anyio = mkDep "anyio" "4.11.0" [EnvProduction, EnvDevelopment]

certifi :: Dependency
certifi = mkDep "certifi" "2025.10.5" [EnvProduction]

h11 :: Dependency
h11 = mkDep "h11" "0.16.0" [EnvProduction]

httpcore :: Dependency
httpcore = mkDep "httpcore" "1.0.9" [EnvProduction]

httpx :: Dependency
httpx = mkDep "httpx" "0.28.1" [EnvProduction]

idna :: Dependency
idna = mkDep "idna" "3.11" [EnvProduction, EnvDevelopment]

sniffio :: Dependency
sniffio = mkDep "sniffio" "1.3.1" [EnvProduction, EnvDevelopment]

starlette :: Dependency
starlette = mkDep "starlette" "0.48.0" [EnvDevelopment]

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "should build correct graph" $ do
      let result = buildGraph Nothing lock

      expectDirect [dep1, dep2, dep3, dep4] result
      expectDeps [dep1, dep2, dep3, dep4, dep5, dep6] result
      expectEdges [(dep1, dep4), (dep3, dep6), (dep4, dep5)] result

    it "should build correct graph using new style dev deps" $ do
      let result = buildGraph Nothing lockNewStyleDevDeps

      expectDirect [dep1, dep3] result
      expectDeps [dep1, dep3, dep6] result
      expectEdges [(dep3, dep6)] result

    it "should skip editable packages without a version" $ do
      let result = buildGraph Nothing lockEditableNoVersion

      expectDirect [dep1, dep3] result
      expectDeps [dep1, dep3, dep6] result
      expectEdges [(dep3, dep6)] result

    it "should handle directory source dependencies" $ do
      let result = buildGraph Nothing lockWithDirectory

      expectDirect [dep1, localLib] result
      expectDeps [dep1, localLib] result
      expectEdges [] result

    it "should label each locked version with the environment that references it" $ do
      let result = buildGraph Nothing lockSplitVersions
      let fooProd = mkDep "foo" "1.0.0" [EnvProduction]
      let fooDev = mkDep "foo" "2.0.0" [EnvDevelopment]
      let bar = mkDep "bar" "1.0.0" [EnvDevelopment]

      expectDirect [fooProd, fooDev] result
      expectDeps [fooProd, fooDev, bar] result
      expectEdges [(fooDev, bar)] result

  describe "parse uv.lock" $ do
    it' "correctly parse and interpret uv.lock" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv.lock|]
      uvlock <- readContentsToml path
      let result = buildGraph Nothing uvlock

      expectDirect' [httpx, starlette] result
      expectDeps'
        [anyio, certifi, h11, httpcore, httpx, idna, sniffio, starlette]
        result
      expectEdges'
        [ (httpx, anyio)
        , (httpx, certifi)
        , (httpx, httpcore)
        , (httpx, idna)
        , (anyio, idna)
        , (anyio, sniffio)
        , (httpcore, certifi)
        , (httpcore, h11)
        , (starlette, anyio)
        ]
        result

    it' "correctly parse uv.lock with editable package missing version" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv-editable.lock|]
      uvlock <- readContentsToml path
      let result = buildGraph Nothing uvlock
      -- In the editable lockfile, anyio and idna are only reachable via httpx (prod),
      -- so they should only have EnvProduction (unlike the main test where starlette
      -- provides a dev path to anyio).
      let anyio' = mkDep "anyio" "4.11.0" [EnvProduction]
      let idna' = mkDep "idna" "3.11" [EnvProduction]

      expectDirect' [httpx, sniffio] result
      expectDeps'
        [anyio', httpx, idna', sniffio]
        result
      expectEdges'
        [ (httpx, anyio')
        , (httpx, idna')
        , (anyio', idna')
        , (anyio', sniffio)
        ]
        result

    it' "correctly parse uv.lock with directory source dependency" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv-directory.lock|]
      uvlock <- readContentsToml path
      let result = buildGraph Nothing uvlock
      let anyio' = mkDep "anyio" "4.11.0" [EnvProduction]
      let idna' = mkDep "idna" "3.11" [EnvProduction]
      let sniffio' = mkDep "sniffio" "1.3.1" [EnvProduction]

      expectDirect' [anyio', localLib] result
      expectDeps'
        [anyio', idna', localLib, sniffio']
        result
      expectEdges'
        [ (anyio', idna')
        , (anyio', sniffio')
        ]
        result

    it' "resolves dependencies on packages with multiple locked versions" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv-multiple-versions.lock|]
      uvlock <- readContentsToml path
      let result = buildGraph Nothing uvlock
      let numpyOld = mkDep "numpy" "2.2.6" [EnvProduction]
      let numpyNew = mkDep "numpy" "2.4.6" [EnvProduction]
      let pandasOld = mkDep "pandas" "2.3.3" [EnvProduction]
      let pandasNew = mkDep "pandas" "3.0.3" [EnvProduction]
      let pytestOld = mkDep "pytest" "8.4.2" [EnvDevelopment]
      let pytestNew = mkDep "pytest" "9.0.3" [EnvDevelopment]
      let pytz = mkDep "pytz" "2026.2" [EnvProduction]
      let tomli = mkDep "tomli" "2.4.1" [EnvDevelopment]

      expectDirect' [numpyOld, numpyNew, pandasOld, pandasNew, pytestOld, pytestNew] result
      expectDeps'
        [numpyOld, numpyNew, pandasOld, pandasNew, pytestOld, pytestNew, pytz, tomli]
        result
      expectEdges'
        [ (pandasOld, numpyOld)
        , (pandasOld, pytz)
        , (pandasNew, numpyNew)
        , (pytestOld, tomli)
        ]
        result

  -- uv-groups.lock and uv-self-reference.lock come from `uv lock` 0.13.7, without the sdist and wheel
  -- entries. uv-groups.lock also leaves out the packages these tests don't look at.
  describe "dependency groups" $ do
    let prod = Set.fromList [EnvProduction]
        dev = Set.fromList [EnvDevelopment]
        prodAndDev = Set.fromList [EnvProduction, EnvDevelopment]

    it' "labels every dependency group development" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv-groups.lock|]
      result <- buildGraph Nothing <$> readContentsToml path

      map dependencyName (directList result) `shouldMatchList'` ["requests", "pytest", "sphinx", "ruff"]
      environments result
        `shouldBe'` Map.fromList
          ( map (,prodAndDev) ["requests", "certifi", "charset-normalizer", "idna", "urllib3"]
              <> map (,dev) ["pytest", "iniconfig", "pygments", "sphinx", "docutils", "jinja2", "markupsafe", "ruff"]
          )

    it' "labels production-groups production" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv-groups.lock|]
      let cfg = UvConfig{uvProductionGroups = Set.fromList ["docs"], uvExcludeGroups = mempty}
      result <- buildGraph (Just cfg) <$> readContentsToml path

      environments result
        `shouldBe'` Map.fromList
          ( map (,prod) ["requests", "certifi", "charset-normalizer", "idna", "urllib3", "sphinx", "docutils", "jinja2", "markupsafe"]
              <> [("pygments", prodAndDev)]
              <> map (,dev) ["pytest", "iniconfig", "ruff"]
          )

    it' "leaves out exclude-groups, keeping packages that production or another group reaches" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv-groups.lock|]
      -- docs is in both lists, so it's left out.
      let cfg = UvConfig{uvProductionGroups = Set.fromList ["docs"], uvExcludeGroups = Set.fromList ["docs", "lint"]}
      result <- buildGraph (Just cfg) <$> readContentsToml path

      map dependencyName (directList result) `shouldMatchList'` ["requests", "pytest"]
      environments result
        `shouldBe'` Map.fromList
          ( map (,prod) ["requests", "certifi", "charset-normalizer", "idna", "urllib3"]
              <> map (,dev) ["pytest", "iniconfig", "pygments"]
          )

    it' "finds the project when one of its groups installs it" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv-self-reference.lock|]
      result <- buildGraph Nothing <$> readContentsToml path

      expectDep' (mkDep "idna" "3.20" [EnvProduction]) result
      expectDep' (mkDep "imagesize" "2.0.1" [EnvDevelopment]) result

    it' "matches configured group names after PEP 503 normalization" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv-self-reference.lock|]
      let cfg = UvConfig{uvProductionGroups = Set.fromList ["Docs_Build"], uvExcludeGroups = mempty}
      result <- buildGraph (Just cfg) <$> readContentsToml path

      expectDep' (mkDep "imagesize" "2.0.1" [EnvProduction]) result
