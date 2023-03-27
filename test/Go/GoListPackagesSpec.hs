{-# LANGUAGE GADTs #-}

module Go.GoListPackagesSpec (
  spec,
) where

import Control.Algebra (run)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Carrier.Stack (runStack)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes (
  DepEnvironment (EnvProduction),
  DepType (GoType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (ExecF (..))
import Effect.Grapher (deep, direct, edges, evalGrapher)
import GraphUtil (expectGraphEqual)
import Graphing qualified (Graphing, direct)
import Path.IO (getCurrentDir)
import ResultUtil (assertOnSuccess)
import Strategy.Go.GoListPackages (GoModule (..), GoPackage (..), ImportPath (..), ModulePath (ModulePath), ModuleVersion (ModuleVersion), analyze, buildGraph)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

type ConstExecC = SimpleC ExecF

runConstExec :: Applicative m => BL.ByteString -> ConstExecC m a -> m a
runConstExec output = interpret $ \case
  Exec{} -> pure (Right output)

clientGolang :: Dependency
clientGolang =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/prometheus/client_golang"
    , dependencyVersion = Just (CEq "v1.12.2")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

xdiff :: Dependency
xdiff =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/ajankovic/xdiff"
    , dependencyVersion = Just (CEq "v0.0.1")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

perks :: Dependency
perks =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/beorn7/perks"
    , dependencyVersion = Just (CEq "v1.0.1")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

protobuf :: Dependency
protobuf =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/golang/protobuf"
    , dependencyVersion = Just (CEq "v1.5.2")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

golangProtobufExtensions :: Dependency
golangProtobufExtensions =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/matttproud/golang_protobuf_extensions"
    , dependencyVersion = Just (CEq "v1.0.1")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

clientModel :: Dependency
clientModel =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/prometheus/client_model"
    , dependencyVersion = Just (CEq "v0.2.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

common :: Dependency
common =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/prometheus/common"
    , dependencyVersion = Just (CEq "v0.32.1")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

procfs :: Dependency
procfs =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/prometheus/procfs"
    , dependencyVersion = Just (CEq "v0.7.3")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

xSys :: Dependency
xSys =
  Dependency
    { dependencyType = GoType
    , dependencyName = "golang.org/x/sys"
    , dependencyVersion = Just (CEq "v0.0.0-20220114195835-da31bd327af9")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

xxhash :: Dependency
xxhash =
  Dependency
    { dependencyType = GoType
    , dependencyName = "github.com/cespare/xxhash/v2"
    , dependencyVersion = Just (CEq "v2.1.2")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

protobufOld :: Dependency
protobufOld =
  Dependency
    { dependencyType = GoType
    , dependencyName = "google.golang.org/protobuf"
    , dependencyVersion = Just (CEq "v1.26.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

expected :: Graphing.Graphing Dependency
expected = run . evalGrapher $ do
  traverse_ direct [clientGolang, xdiff]
  traverse_
    deep
    [ perks
    , protobuf
    , golangProtobufExtensions
    , clientModel
    , common
    , procfs
    , xSys
    , xxhash
    , protobufOld
    ]

  edges
    [ (protobuf, protobufOld)
    , (golangProtobufExtensions, protobuf)
    , (clientModel, protobuf)
    , (common, protobuf)
    , (common, golangProtobufExtensions)
    , (common, clientModel)
    , (procfs, xSys)
    , (clientGolang, perks)
    , (clientGolang, protobuf)
    , (clientGolang, protobufOld)
    , (clientGolang, clientModel)
    , (clientGolang, common)
    , (clientGolang, procfs)
    , (clientGolang, protobuf)
    , (clientGolang, xxhash)
    ]

-- | This FilePath represents =go list -json -deps all= output with a local package replacement and transitive deps.
testFile :: FilePath
testFile = "test/Go/testdata/go-list-pkgs-out.json"

ignoresC :: Spec
ignoresC =
  describe "Using C bindings" $
    it "Ignores the special \"C\" package in dependencies" $ do
      let result =
            run . runStack . runDiagnostics $
              buildGraph
                [ GoPackage
                    { importPath = ImportPath "github.com/ajankovic/xdiff"
                    , standard = False
                    , moduleInfo =
                        Just
                          GoModule
                            { modulePath = ModulePath "github.com/ajankovic/xdiff"
                            , version = Just (ModuleVersion "v0.0.1")
                            , indirect = False
                            , isMainModule = False
                            , replacement = Nothing
                            }
                    , packageDeps = [ImportPath "C"]
                    }
                ]
      assertOnSuccess result $ \_ (graph, _) -> graph `shouldBe` Graphing.direct xdiff

analysisSpec :: Spec
analysisSpec = do
  outputTrivial <- runIO (BL.readFile testFile)
  -- The following dir is ignored because of the ConstExecC carrier.
  testdir <- runIO getCurrentDir

  describe "golist analyze" $ do
    it "produces the expected output" $ do
      let result =
            analyze testdir
              & runConstExec outputTrivial
              & runDiagnostics
              & runStack
              & run
      assertOnSuccess result $ \_ (graph, _) -> graph `expectGraphEqual` expected

spec :: Spec
spec =
  do
    analysisSpec
    ignoresC
