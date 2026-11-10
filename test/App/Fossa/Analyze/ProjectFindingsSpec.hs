{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Analyze.ProjectFindingsSpec (spec) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.Analyze.ProjectFindings (renderProjectFindings)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes (DepEnvironment (..), DepType (MavenType, NodeJSType), Dependency (..), VerConstraint (CEq))
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, Dir, Path, mkAbsDir, toFilePath)
import Test.Hspec (Spec, describe, it, shouldBe)
import Types (DiscoveredProjectType (GradleProjectType, NpmProjectType), GraphBreadth (Complete, Partial))

#ifdef mingw32_HOST_OS
testPath :: Path Abs Dir
testPath = $(mkAbsDir "C:/repo")
#else
testPath :: Path Abs Dir
testPath = $(mkAbsDir "/repo")
#endif

-- | mvn+org.example:a$1.0.0, production-only
depA :: Dependency
depA = Dependency MavenType "org.example:a" (Just (CEq "1.0.0")) [] (Set.fromList [EnvProduction]) Map.empty

-- | mvn+org.example:b$2.0.0, multi-scope including EnvOther
depB :: Dependency
depB = Dependency MavenType "org.example:b" (Just (CEq "2.0.0")) [] (Set.fromList [EnvProduction, EnvTesting, EnvOther "runtimeClasspath"]) Map.empty

-- | npm+c$3.0.0, development-only
depC :: Dependency
depC = Dependency NodeJSType "c" (Just (CEq "3.0.0")) [] (Set.fromList [EnvDevelopment]) Map.empty

-- Two deps that render to the same locator, distinguished only by environment.
depXPkgProduction :: Dependency
depXPkgProduction = Dependency NodeJSType "pkg" (Just (CEq "1.0.0")) [] (Set.fromList [EnvProduction]) Map.empty

depXPkgTesting :: Dependency
depXPkgTesting = Dependency NodeJSType "pkg" (Just (CEq "1.0.0")) [] (Set.fromList [EnvTesting]) Map.empty

mkProject :: DiscoveredProjectType -> Graphing Dependency -> GraphBreadth -> ProjectResult
mkProject projectType graph breadth =
  ProjectResult
    { projectResultType = projectType
    , projectResultPath = testPath
    , projectResultGraph = graph
    , projectResultGraphBreadth = breadth
    , projectResultManifestFiles = []
    }

pathText :: Text
pathText = toText (toFilePath testPath)

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

projectObj :: Text -> Text -> [Aeson.Value] -> [Aeson.Value] -> [Int] -> Aeson.Value
projectObj projectType breadth nodes edges directNodes =
  Aeson.object
    [ "projectType" .= (projectType :: Text)
    , "path" .= pathText
    , "breadth" .= breadth
    , "nodes" .= nodes
    , "edges" .= edges
    , "directNodes" .= (directNodes :: [Int])
    ]

spec :: Spec
spec = do
  describe "renderProjectFindings" $ do
    it "renders a multi-scope direct dependency including EnvOther" $ do
      let graph = Graphing.directs [depB]
          expected = Aeson.object ["projects" .= [projectObj "gradle" "GRAPH_BREADTH_COMPLETE" [nodeObj "mvn+org.example:b$2.0.0" ["other:runtimeClasspath", "production", "testing"]] [] [0]]]
      renderProjectFindings [mkProject GradleProjectType graph Complete] `shouldBe` expected

    it "renders direct and transitive nodes with deduplicated, sorted index edges" $ do
      let graph = Graphing.directs [depA, depB] <> Graphing.edges [(depA, depC), (depB, depC), (depA, depC)]
          -- Sorted by locator: a=0, b=1, c=2
          expected = Aeson.object [projectsPair]
            where
              projectsPair =
                "projects"
                  .= [ projectObj
                         "gradle"
                         "GRAPH_BREADTH_COMPLETE"
                         [ nodeObj "mvn+org.example:a$1.0.0" ["production"]
                         , nodeObj "mvn+org.example:b$2.0.0" ["other:runtimeClasspath", "production", "testing"]
                         , nodeObj "npm+c$3.0.0" ["development"]
                         ]
                         [edgeObj 0 2, edgeObj 1 2]
                         [0, 1]
                     ]
      renderProjectFindings [mkProject GradleProjectType graph Complete] `shouldBe` expected

    it "keeps two distinct nodes that share one locator" $ do
      let graph = Graphing.directs [depXPkgProduction, depXPkgTesting]
          expected = Aeson.object [projectsPair]
            where
              projectsPair =
                "projects"
                  .= [ projectObj
                         "npm"
                         "GRAPH_BREADTH_COMPLETE"
                         [ nodeObj "npm+pkg$1.0.0" ["production"]
                         , nodeObj "npm+pkg$1.0.0" ["testing"]
                         ]
                         []
                         [0, 1]
                     ]
      renderProjectFindings [mkProject NpmProjectType graph Complete] `shouldBe` expected

    it "renders both breadth values and keeps project list order" $ do
      let graph = Graphing.directs [depA]
          expected = Aeson.object [projectsPair]
            where
              projectsPair =
                "projects"
                  .= [ projectObj "gradle" "GRAPH_BREADTH_COMPLETE" [nodeObj "mvn+org.example:a$1.0.0" ["production"]] [] [0]
                     , projectObj "npm" "GRAPH_BREADTH_PARTIAL" [nodeObj "mvn+org.example:a$1.0.0" ["production"]] [] [0]
                     ]
      renderProjectFindings
        [ mkProject GradleProjectType graph Complete
        , mkProject NpmProjectType graph Partial
        ]
        `shouldBe` expected

    it "is deterministic regardless of node insertion order" $ do
      let project1 = mkProject GradleProjectType (Graphing.directs [depA, depB] <> Graphing.edges [(depA, depC), (depB, depC)]) Complete
          project2 = mkProject GradleProjectType (Graphing.directs [depB, depA] <> Graphing.edges [(depB, depC), (depA, depC)]) Complete
      renderProjectFindings [project1] `shouldBe` renderProjectFindings [project2]
