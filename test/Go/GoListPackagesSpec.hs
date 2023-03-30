module Go.GoListPackagesSpec (
  spec,
)
where

import Control.Algebra (run)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes (
  DepEnvironment (EnvProduction),
  DepType (GoType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (expectGraphEqual)
import Graphing qualified (Graphing, direct, edge)
import ResultUtil (assertOnSuccess)
import Strategy.Go.GoListPackages (GoModule (..), GoPackage (..), ImportPath (..), ModulePath (ModulePath), ModuleVersion (ModuleVersion), buildGraph)
import Test.Hspec (Spec, describe, it)

-- These packages are set up to test the following features:
--
-- 1. Direct/deep/transitive deps.
-- 2. Local path replacement.
-- 3. Module replacement.
-- 4. Eliminating the C special package.
-- 5. Eliminating path dependencies and their transitive deps.
testPackages :: [GoPackage]
testPackages =
  [ GoPackage
      { importPath = ImportPath "main/pkg1"
      , standard = False
      , moduleInfo =
          Just
            GoModule
              { modulePath = ModulePath "main"
              , version = Just (ModuleVersion "1.0.0")
              , indirect = False
              , isMainModule = True
              , replacement = Nothing
              }
      , packageDeps =
          [ ImportPath "moduleA/directDep"
          , -- C is a special package for use with Go's FFI.
            -- It should be totally ignored by the graphing function.
            ImportPath "C"
          ]
      , listError = Nothing
      , testDeps = []
      }
  , GoPackage
      { importPath = ImportPath "moduleA/directDep"
      , standard = False
      , moduleInfo =
          Just
            GoModule
              { modulePath = ModulePath "moduleA"
              , version = Just (ModuleVersion "1.0.0")
              , indirect = False
              , isMainModule = False
              , replacement = Nothing
              }
      , packageDeps = []
      , listError = Nothing
      , testDeps = []
      }
  , -- The following is a module that should be replaced with another module.
    GoPackage
      { importPath = ImportPath "replacedModule/pkg1"
      , standard = False
      , moduleInfo =
          Just
            GoModule
              { modulePath = ModulePath "replacedModule"
              , version = Just (ModuleVersion "1.0.0")
              , indirect = False
              , isMainModule = False
              , replacement =
                  Just
                    GoModule
                      { modulePath = ModulePath "moduleReplacement"
                      , version = Just (ModuleVersion "2.0.0")
                      , indirect = False
                      , isMainModule = False
                      , replacement = Nothing
                      }
              }
      , -- Even with replacements, dependencies still appear in the graph
        packageDeps = [ImportPath "moduleA/directDep"]
      , listError = Nothing
      , testDeps = []
      }
  , -- The following is a module that should be replaced with a path dep.
    -- It should be removed by the graphing function because it's a path dep.
    GoPackage
      { importPath = ImportPath "pathDepReplaced/pkg1"
      , standard = False
      , moduleInfo =
          Just
            GoModule
              { modulePath = ModulePath "pathDepReplaced"
              , version = Just (ModuleVersion "1.0.0")
              , indirect = True
              , isMainModule = False
              , replacement =
                  Just
                    GoModule
                      { modulePath = ModulePath "../local_package"
                      , version = Just (ModuleVersion "1.0.0")
                      , indirect = False
                      , isMainModule = False
                      , replacement = Nothing
                      }
              }
      , packageDeps = [ImportPath "pathDepDependency/pkg1"]
      , listError = Nothing
      , testDeps = []
      }
  , -- The following is a module that is depended on only by a path dep.
    -- It should not appear in the result graph.
    GoPackage
      { importPath = ImportPath "pathDepDependency/pkg1"
      , standard = False
      , moduleInfo =
          Just
            GoModule
              { modulePath = ModulePath "pathDepDependency"
              , version = Just (ModuleVersion "1.0.0")
              , indirect = True
              , isMainModule = False
              , replacement = Nothing
              }
      , packageDeps = []
      , listError = Nothing
      , testDeps = []
      }
  ]

moduleA :: Dependency
moduleA =
  Dependency
    { dependencyType = GoType
    , dependencyName = "moduleA"
    , dependencyVersion = Just $ CEq "1.0.0"
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

replacedModule :: Dependency
replacedModule =
  Dependency
    { dependencyType = GoType
    , dependencyName = "moduleReplacement"
    , dependencyVersion = Just $ CEq "2.0.0"
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

expectedGraph :: Graphing.Graphing Dependency
expectedGraph =
  Graphing.direct replacedModule
    <> Graphing.direct moduleA
    <> Graphing.edge replacedModule moduleA

buildGraphSpec :: Spec
buildGraphSpec = it "Graphs modules based on package dependencies" $ do
  let result = run . runStack . runDiagnostics . buildGraph $ testPackages
  assertOnSuccess result $ \_ (graph, _) -> graph `expectGraphEqual` expectedGraph

spec :: Spec
spec = describe "Graphing deps with go list -json -deps all" buildGraphSpec
