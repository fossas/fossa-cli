{-# LANGUAGE QuasiQuotes #-}

module Python.UvSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import Effect.ReadFS (readContentsToml)
import GraphUtil
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
            , uvlockPackageDependencies = ["dep1", "dep2"]
            , uvlockPackageDevDependencies = ["dep3", "dep4"]
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = Just "1.1.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = ["dep4"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep2"
            , uvlockPackageVersion = Just "2.1.2"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep3"
            , uvlockPackageVersion = Just "0.9.9"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = ["dep6"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep4"
            , uvlockPackageVersion = Just "1.0.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = ["dep5"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep5"
            , uvlockPackageVersion = Just "3.0.1"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep6"
            , uvlockPackageVersion = Just "1.1.1"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
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
            , uvlockPackageDependencies = ["dep1", "dep2"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = Map.fromList [("dev", ["dep3"])]
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = Just "1.1.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep3"
            , uvlockPackageVersion = Just "0.9.9"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = ["dep6"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep6"
            , uvlockPackageVersion = Just "1.1.1"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
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
            , uvlockPackageDependencies = ["dep1"]
            , uvlockPackageDevDependencies = ["dep3"]
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = Just "1.1.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep3"
            , uvlockPackageVersion = Just "0.9.9"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = ["dep6"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep6"
            , uvlockPackageVersion = Just "1.1.1"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
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
            , uvlockPackageDependencies = ["dep1", "local-lib"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = Just "1.1.0"
            , uvlockPackageSource = SourceRegistry "https://pypi.org/simple"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "local-lib"
            , uvlockPackageVersion = Just "0.2.0"
            , uvlockPackageSource = SourceDirectory "../local-lib"
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        ]
    }

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
      let result = buildGraph lock

      expectDirect [dep1, dep2, dep3, dep4] result
      expectDeps [dep1, dep2, dep3, dep4, dep5, dep6] result
      expectEdges [(dep1, dep4), (dep3, dep6), (dep4, dep5)] result

    it "should build correct graph using new style dev deps" $ do
      let result = buildGraph lockNewStyleDevDeps

      expectDirect [dep1, dep3] result
      expectDeps [dep1, dep3, dep6] result
      expectEdges [(dep3, dep6)] result

    it "should skip editable packages without a version" $ do
      let result = buildGraph lockEditableNoVersion

      expectDirect [dep1, dep3] result
      expectDeps [dep1, dep3, dep6] result
      expectEdges [(dep3, dep6)] result

    it "should handle directory source dependencies" $ do
      let result = buildGraph lockWithDirectory

      expectDirect [dep1, localLib] result
      expectDeps [dep1, localLib] result
      expectEdges [] result

  describe "parse uv.lock" $ do
    it' "correctly parse and interpret uv.lock" $ do
      path <- makeAbsolute [relfile|test/Python/testdata/uv.lock|]
      uvlock <- readContentsToml path
      let result = buildGraph uvlock

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
      let result = buildGraph uvlock
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
      let result = buildGraph uvlock
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
