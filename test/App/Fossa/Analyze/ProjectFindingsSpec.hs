module App.Fossa.Analyze.ProjectFindingsSpec (spec) where

import App.Fossa.Analyze (WorkflowUploadPayload (..), clobbersProjectFindings, projectFindingsPayload)
import App.Fossa.Analyze.ForkAlias (mkForkAliasMap)
import App.Fossa.Analyze.Project (ProjectResult (..), PublishedGraph (PublishedGraph), RawGraph (RawGraph))
import App.Fossa.Analyze.ProjectFindings (renderProjectFindings)
import App.Fossa.ManualDeps (ForkAlias (..), ForkAliasEntry (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (fromText)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (DepEnvironment (..), DepType (CargoType, GoType, MavenType, NodeJSType), Dependency (..), VerConstraint (CEq))
import Graphing qualified
import Path (parseAbsDir, toFilePath)
import Srclib.Types (Locator)
import System.Directory (getTemporaryDirectory)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Types (DiscoveredProjectType (CargoProjectType, GradleProjectType, NpmProjectType), GraphBreadth (Complete, Partial))

-- | mvn+org.example:a$1.0.0, production-only
depA :: Dependency
depA = Dependency MavenType "org.example:a" (Just (CEq "1.0.0")) [] (Set.fromList [EnvProduction]) Map.empty

depB :: Dependency
depB = Dependency MavenType "org.example:b" (Just (CEq "2.0.0")) [] (Set.fromList [EnvProduction, EnvTesting, EnvOther "runtimeClasspath"]) Map.empty

depC :: Dependency
depC = Dependency NodeJSType "c" (Just (CEq "3.0.0")) [] (Set.fromList [EnvDevelopment]) Map.empty

depLost :: Dependency
depLost = Dependency GoType "github.com/lost/deep" (Just (CEq "9.9.9")) [] (Set.fromList [EnvProduction]) Map.empty

depForked :: Dependency
depForked = Dependency CargoType "my-serde" (Just (CEq "1.0.0")) [] (Set.fromList [EnvProduction]) Map.empty

depSerdeDirect :: Dependency
depSerdeDirect = Dependency CargoType "serde" (Just (CEq "2.0.0")) [] (Set.fromList [EnvProduction]) Map.empty

-- No environments: no occurrence rows can exist.
depNoEnv :: Dependency
depNoEnv = Dependency MavenType "org.example:none" (Just (CEq "0.1.0")) [] Set.empty Map.empty

depXPkgProduction :: Dependency
depXPkgProduction = Dependency NodeJSType "pkg" (Just (CEq "1.0.0")) [] (Set.fromList [EnvProduction]) Map.empty

depXPkgTesting :: Dependency
depXPkgTesting = Dependency NodeJSType "pkg" (Just (CEq "1.0.0")) [] (Set.fromList [EnvTesting]) Map.empty

serdeAliasMap :: Map.Map Locator ForkAlias
serdeAliasMap =
  mkForkAliasMap
    [ ForkAlias
        { forkAliasFork = ForkAliasEntry{forkAliasEntryType = CargoType, forkAliasEntryName = "my-serde", forkAliasEntryVersion = Nothing}
        , forkAliasBase = ForkAliasEntry{forkAliasEntryType = CargoType, forkAliasEntryName = "serde", forkAliasEntryVersion = Just "2.0.0"}
        , forkAliasLabels = []
        }
    ]

-- The findings document the CLI contributes to the upload payload.
findingsDoc :: Aeson.Value
findingsDoc = Aeson.object ["projects" .= ([] :: [Aeson.Value])]

nodeObj :: Text -> [Text] -> Aeson.Value
nodeObj locator environments =
  Aeson.object
    [ "locator" .= (locator :: Text)
    , "environments" .= environments
    ]

edgeObj :: Int -> Int -> Aeson.Value
edgeObj fromIndex toIndex =
  Aeson.object
    [ "fromIndex" .= (fromIndex :: Int)
    , "toIndex" .= toIndex
    ]

occObj :: Text -> Bool -> Text -> Aeson.Value
occObj modulePath isDirect scope =
  Aeson.object
    [ "module" .= modulePath
    , "isDirect" .= (isDirect :: Bool)
    , "scope" .= scope
    ]

-- aliasedFrom present only when a fork alias changed the dependency's locator.
occObjAliased :: Text -> Bool -> Text -> Text -> Aeson.Value
occObjAliased modulePath isDirect scope aliasedFrom =
  Aeson.object
    [ "aliasedFrom" .= (aliasedFrom :: Text)
    , "module" .= modulePath
    , "isDirect" .= (isDirect :: Bool)
    , "scope" .= scope
    ]

depsObject :: [(Text, [Aeson.Value])] -> Aeson.Value
depsObject pairs = Aeson.object $ map (\(locator, rows) -> (fromText locator, Aeson.object ["occurrences" .= rows])) pairs

graphObject :: [Aeson.Value] -> [Aeson.Value] -> [Int] -> Aeson.Value
graphObject nodes edges directNodes =
  Aeson.object
    [ "nodes" .= nodes
    , "edges" .= edges
    , "directNodes" .= (directNodes :: [Int])
    ]

projectObj :: Text -> Text -> Text -> Aeson.Value -> Aeson.Value -> Aeson.Value
projectObj projectPath projectType breadth dependencies graph =
  Aeson.object
    [ "projectType" .= (projectType :: Text)
    , "path" .= projectPath
    , "breadth" .= breadth
    , "dependencies" .= dependencies
    , "graph" .= graph
    ]

spec :: Spec
spec = do
  -- Parsed at runtime so the spec compiles and runs on both Windows and POSIX
  -- without CPP; the path package rejects the other platform's spellings.
  testPath <- runIO (getTemporaryDirectory >>= parseAbsDir)
  let -- Mirrors mkResult: 'projectResultGraph' carries the pruned (published)
      -- view, so findings assertions would fail if the renderer read that field.
      mkProject projectType unpruned breadth =
        ProjectResult
          { projectResultType = projectType
          , projectResultPath = testPath
          , projectResultUnprunedGraph = RawGraph unpruned
          , projectResultGraph = PublishedGraph (Graphing.pruneUnreachable unpruned)
          , projectResultGraphBreadth = breadth
          , projectResultManifestFiles = []
          }
  describe "renderProjectFindings" $ do
    it "captures unreachable nodes that pruneUnreachable would delete (pre-prune resolution)" $ do
      let graph = Graphing.directs [depA] <> Graphing.deeps [depLost]
          obj =
            projectObj
              ""
              "gradle"
              "GRAPH_BREADTH_COMPLETE"
              ( depsObject
                  [ ("go+github.com/lost/deep$9.9.9", [occObj "" False "production"])
                  , ("mvn+org.example:a$1.0.0", [occObj "" True "production"])
                  ]
              )
              (graphObject [nodeObj "go+github.com/lost/deep$9.9.9" ["production"], nodeObj "mvn+org.example:a$1.0.0" ["production"]] [] [1])
          expected = Aeson.object ["projects" .= [obj]]
      -- Sanity pin: the published graph no longer contains depLost, so the
      -- renderer must be reading the unpruned field for this to hold.
      Graphing.hasVertex depLost (Graphing.pruneUnreachable graph) `shouldBe` False
      renderProjectFindings testPath Map.empty [mkProject GradleProjectType graph Complete] `shouldBe` expected

    it "emits one occurrence row per environment, module mirrors path, isDirect from the graph roots" $ do
      let graph = Graphing.directs [depB] <> Graphing.edges [(depB, depC)]
          obj =
            -- No fork alias applies to these locators: aliasedFrom absent even with a non-empty map.
            projectObj
              ""
              "gradle"
              "GRAPH_BREADTH_COMPLETE"
              ( depsObject
                  [ ("mvn+org.example:b$2.0.0", [occObj "" True "other:runtimeClasspath", occObj "" True "production", occObj "" True "testing"])
                  , ("npm+c$3.0.0", [occObj "" False "development"])
                  ]
              )
              (graphObject [nodeObj "mvn+org.example:b$2.0.0" ["other:runtimeClasspath", "production", "testing"], nodeObj "npm+c$3.0.0" ["development"]] [edgeObj 0 1] [0])
          expected = Aeson.object ["projects" .= [obj]]
      renderProjectFindings testPath serdeAliasMap [mkProject GradleProjectType graph Complete] `shouldBe` expected

    it "uses the translated locator as key and records aliasedFrom only when translation applied" $ do
      let graph = Graphing.directs [depA, depForked]
          obj =
            projectObj
              ""
              "cargo"
              "GRAPH_BREADTH_COMPLETE"
              ( depsObject
                  [ ("cargo+serde$2.0.0", [occObjAliased "" True "production" "cargo+my-serde$1.0.0"])
                  , ("mvn+org.example:a$1.0.0", [occObj "" True "production"])
                  ]
              )
              (graphObject [nodeObj "cargo+serde$2.0.0" ["production"], nodeObj "mvn+org.example:a$1.0.0" ["production"]] [] [0, 1])
          expected = Aeson.object ["projects" .= [obj]]
      renderProjectFindings testPath serdeAliasMap [mkProject CargoProjectType graph Complete] `shouldBe` expected

    it "keeps two distinct nodes that share one locator and merges their occurrences on scope" $ do
      let graph = Graphing.directs [depXPkgProduction, depXPkgTesting]
          obj =
            projectObj
              ""
              "npm"
              "GRAPH_BREADTH_COMPLETE"
              (depsObject [("npm+pkg$1.0.0", [occObj "" True "production", occObj "" True "testing"])])
              (graphObject [nodeObj "npm+pkg$1.0.0" ["production"], nodeObj "npm+pkg$1.0.0" ["testing"]] [] [0, 1])
          expected = Aeson.object ["projects" .= [obj]]
      renderProjectFindings testPath Map.empty [mkProject NpmProjectType graph Complete] `shouldBe` expected

    -- The only merge-path exercise: both nodes translate to cargo+serde$2.0.0
    -- with the same environment, so their rows combine on scope. The raw-locator
    -- tiebreak sorts the forked dep first, so its aliasedFrom is the survivor.
    it "merges two aliased nodes sharing one locator and environment, ORing isDirect and keeping the first aliasedFrom" $ do
      let graph = Graphing.directs [depSerdeDirect] <> Graphing.deeps [depForked]
          obj =
            projectObj
              ""
              "cargo"
              "GRAPH_BREADTH_COMPLETE"
              (depsObject [("cargo+serde$2.0.0", [occObjAliased "" True "production" "cargo+my-serde$1.0.0"])])
              (graphObject [nodeObj "cargo+serde$2.0.0" ["production"], nodeObj "cargo+serde$2.0.0" ["production"]] [] [1])
          expected = Aeson.object ["projects" .= [obj]]
      renderProjectFindings testPath serdeAliasMap [mkProject CargoProjectType graph Complete] `shouldBe` expected

    it "renders project paths relative to the scan target, empty at the target itself" $ do
      subdir <- parseAbsDir (toFilePath testPath <> "packages/cli/")
      let graph = Graphing.directs [depA]
          obj =
            projectObj
              "packages/cli"
              "gradle"
              "GRAPH_BREADTH_COMPLETE"
              (depsObject [("mvn+org.example:a$1.0.0", [occObj "packages/cli" True "production"])])
              (graphObject [nodeObj "mvn+org.example:a$1.0.0" ["production"]] [] [0])
          atRoot =
            projectObj
              ""
              "gradle"
              "GRAPH_BREADTH_COMPLETE"
              (depsObject [("mvn+org.example:a$1.0.0", [occObj "" True "production"])])
              (graphObject [nodeObj "mvn+org.example:a$1.0.0" ["production"]] [] [0])
          expected = Aeson.object ["projects" .= [obj, atRoot]]
          mkProjectAt path = (mkProject GradleProjectType graph Complete){projectResultPath = path}
      renderProjectFindings testPath Map.empty [mkProjectAt subdir, mkProjectAt testPath] `shouldBe` expected

    it "renders deduplicated sorted index edges and both breadth values, keeping project list order" $ do
      let graph = Graphing.directs [depA, depB] <> Graphing.edges [(depA, depC), (depB, depC), (depA, depC)]
          obj1 =
            projectObj
              ""
              "gradle"
              "GRAPH_BREADTH_COMPLETE"
              ( depsObject
                  [ ("mvn+org.example:a$1.0.0", [occObj "" True "production"])
                  , ("mvn+org.example:b$2.0.0", [occObj "" True "other:runtimeClasspath", occObj "" True "production", occObj "" True "testing"])
                  , ("npm+c$3.0.0", [occObj "" False "development"])
                  ]
              )
              ( graphObject
                  [ nodeObj "mvn+org.example:a$1.0.0" ["production"]
                  , nodeObj "mvn+org.example:b$2.0.0" ["other:runtimeClasspath", "production", "testing"]
                  , nodeObj "npm+c$3.0.0" ["development"]
                  ]
                  [edgeObj 0 2, edgeObj 1 2]
                  [0, 1]
              )
          obj2 =
            projectObj
              ""
              "npm"
              "GRAPH_BREADTH_PARTIAL"
              (depsObject [("mvn+org.example:a$1.0.0", [occObj "" True "production"])])
              (graphObject [nodeObj "mvn+org.example:a$1.0.0" ["production"]] [] [0])
          expected = Aeson.object ["projects" .= [obj1, obj2]]
      renderProjectFindings testPath Map.empty [mkProject GradleProjectType graph Complete, mkProject NpmProjectType (Graphing.directs [depA]) Partial] `shouldBe` expected

    it "omits env-less locators from the occurrence map but keeps them as graph nodes" $ do
      let graph = Graphing.directs [depNoEnv]
          obj =
            projectObj
              ""
              "gradle"
              "GRAPH_BREADTH_COMPLETE"
              (depsObject [])
              (graphObject [nodeObj "mvn+org.example:none$0.1.0" []] [] [0])
          expected = Aeson.object ["projects" .= [obj]]
      renderProjectFindings testPath Map.empty [mkProject GradleProjectType graph Complete] `shouldBe` expected

    it "is deterministic regardless of graph construction order" $ do
      let project1 = mkProject GradleProjectType (Graphing.directs [depA, depB] <> Graphing.edges [(depA, depC), (depB, depC)]) Complete
          project2 = mkProject GradleProjectType (Graphing.directs [depB, depA] <> Graphing.edges [(depB, depC), (depA, depC)]) Complete
      renderProjectFindings testPath serdeAliasMap [project1] `shouldBe` renderProjectFindings testPath serdeAliasMap [project2]

  describe "workflow upload payload merge" $ do
    it "unions two object payloads with the right key winning and the left's other keys preserved" $ do
      let left = WorkflowUploadPayload $ Aeson.object [fromText "keep" .= (1 :: Int), fromText "projectFindings" .= ("from-workflow" :: Text)]
          right = projectFindingsPayload findingsDoc
          merged = unWorkflowUploadPayload (left <> right)
          expected = Aeson.object [fromText "keep" .= (1 :: Int), fromText "projectFindings" .= findingsDoc]
      merged `shouldBe` expected

    it "lets a non-object right payload win outright" $ do
      let left = WorkflowUploadPayload $ Aeson.object [fromText "keep" .= (1 :: Int)]
          right = WorkflowUploadPayload $ Aeson.toJSON ("not-an-object" :: Text)
      unWorkflowUploadPayload (left <> right) `shouldBe` Aeson.toJSON ("not-an-object" :: Text)

  describe "clobbersProjectFindings" $ do
    it "is True when the workflow payload is an object carrying projectFindings" $
      clobbersProjectFindings (Just findingsDoc) (Just $ Aeson.object [fromText "projectFindings" .= ("from-workflow" :: Text)]) `shouldBe` True

    it "is False when the workflow payload is not an object" $
      clobbersProjectFindings (Just findingsDoc) (Just $ Aeson.toJSON ("not-an-object" :: Text)) `shouldBe` False

    it "is False when the analysis produced no findings" $
      clobbersProjectFindings Nothing (Just $ Aeson.object [fromText "projectFindings" .= ("from-workflow" :: Text)]) `shouldBe` False

    it "is False when the workflow payload has no projectFindings key" $
      clobbersProjectFindings (Just findingsDoc) (Just $ Aeson.object [fromText "schemaVersion" .= (1 :: Int)]) `shouldBe` False
