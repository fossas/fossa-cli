module Python.UvSpec (
  spec,
) where

import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import GraphUtil
import Strategy.Python.Uv
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
            }
        , UvLockPackage
            { uvlockPackageName = "dep1"
            , uvlockPackageVersion = "1.1.0"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = ["dep4"]
            , uvlockPackageDevDependencies = []
            }
        , UvLockPackage
            { uvlockPackageName = "dep2"
            , uvlockPackageVersion = "2.1.2"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            }
        , UvLockPackage
            { uvlockPackageName = "dep3"
            , uvlockPackageVersion = "0.9.9"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = ["dep6"]
            , uvlockPackageDevDependencies = []
            }
        , UvLockPackage
            { uvlockPackageName = "dep4"
            , uvlockPackageVersion = "1.0.0"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = ["dep5"]
            , uvlockPackageDevDependencies = []
            }
        , UvLockPackage
            { uvlockPackageName = "dep5"
            , uvlockPackageVersion = "3.0.1"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
            }
        , UvLockPackage
            { uvlockPackageName = "dep6"
            , uvlockPackageVersion = "1.1.1"
            , uvlockPackageSource = UvLockPackageSource Nothing
            , uvlockPackageDependencies = []
            , uvlockPackageDevDependencies = []
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

spec :: Spec
spec = do
  describe "buildGraph" $
    it "should build correct graph" $ do
      let result = buildGraph lock

      expectDirect [dep1, dep2, dep3, dep4] result
      expectDeps [dep1, dep2, dep3, dep4, dep5, dep6] result
      expectEdges [(dep1, dep4), (dep3, dep6), (dep4, dep5)] result
