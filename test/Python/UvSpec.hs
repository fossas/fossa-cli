{-# LANGUAGE QuasiQuotes #-}

module Python.UvSpec (
  spec,
) where

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
            , uvlockPackageVersion = "0.1.0"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = ["dep1", "dep2"]
            , uvlockPackageDevDependencies = ["dep3", "dep4"]
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = "1.1.0"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = ["dep4"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep2"
            , uvlockPackageVersion = "2.1.2"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep3"
            , uvlockPackageVersion = "0.9.9"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = ["dep6"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep4"
            , uvlockPackageVersion = "1.0.0"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = ["dep5"]
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep5"
            , uvlockPackageVersion = "3.0.1"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            , uvlockPackageOptionalDependencies = mempty
            }
        , UvLockPackage
            { uvlockPackageName = "dep6"
            , uvlockPackageVersion = "1.1.1"
            , uvlockPackageSource = UvLockPackageSource Nothing
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
