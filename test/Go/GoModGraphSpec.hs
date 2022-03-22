module Go.GoModGraphSpec (spec) where

import Data.Map qualified as Map
import Data.SemVer (version)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import DepTypes (
  DepType (GoType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Go.GoModGraph (GoBuildGraphCfg (..), GoGraphMod (..), GoModReplacements (GoModReplacements), MVSOpt (ApplyMVS, NoMVS), buildGraph, parseGoGraphMod, parseGoModGraph)
import Strategy.Go.Gomod (PackageVersion (NonCanonical, Pseudo, Semantic))
import Test.Hspec (Expectation, Spec, context, describe, it, runIO)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse, runParser)

semver :: Int -> Int -> Int -> PackageVersion
semver x y z = Semantic $ version x y z [] []

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

mkGoMod :: Text -> Text -> GoGraphMod
mkGoMod name ver = OtherMod name (NonCanonical ver)

mkDep :: Text -> Text -> Dependency
mkDep name ver = Dependency GoType name (Just $ CEq ver) [] mempty Map.empty

spec :: Spec
spec = do
  specParse
  specGraph

specGraph :: Spec
specGraph = do
  describe "buildGraph" $ do
    -- Test case graph is from: https://golang.org/ref/mod#minimal-version-selection (first diagram)
    let testEdges =
          [ (MainMod "Main", mkGoMod "A" "1.2")
          , (MainMod "Main", mkGoMod "B" "1.2")
          , -- from A
            (mkGoMod "A" "1.1", mkGoMod "C" "1.1")
          , (mkGoMod "A" "1.2", mkGoMod "C" "1.3")
          , -- from B
            (mkGoMod "B" "1.2", mkGoMod "C" "1.4")
          , (mkGoMod "B" "1.3", mkGoMod "E" "1.1")
          , -- from C
            (mkGoMod "C" "1.1", mkGoMod "D" "1.1")
          , (mkGoMod "C" "1.2", mkGoMod "D" "1.1")
          , (mkGoMod "C" "1.3", mkGoMod "D" "1.2")
          , (mkGoMod "C" "1.4", mkGoMod "D" "1.2")
          , -- from E
            (mkGoMod "E" "1.1", mkGoMod "F" "1.1")
          , -- from F
            (mkGoMod "F" "1.1", mkGoMod "E" "1.1")
          ]

    let testSelectedMods =
          Set.fromList
            [ mkGoMod "A" "1.2"
            , mkGoMod "B" "1.2"
            , mkGoMod "C" "1.4"
            , mkGoMod "D" "1.2"
            ]
    let directMods = Set.fromList [mkGoMod "A" "1.2", mkGoMod "B" "1.2"]

    let mkModBuildCfg replacements mvs =
          GoBuildGraphCfg
            { selectedMods = testSelectedMods
            , mainMod = MainMod "Main"
            , directMods = directMods
            , modReplacements = replacements
            , mvsOption = mvs
            , goModGraphOutput = testEdges
            }

    context "Should generate a graph with replacements applied" $ do
      let replacements =
            GoModReplacements $
              Map.fromList
                [ (mkGoMod "A" "1.2", mkGoMod "A" "1.6")
                , (mkGoMod "D" "1.2", mkGoMod "D" "2.0")
                ]

      let graph = buildGraph (mkModBuildCfg replacements ApplyMVS)
      -- Assert
      let depAReplaced = mkDep "A" "1.6"
      let depB = mkDep "B" "1.2"
      let depC = mkDep "C" "1.4"
      let depDReplaced = mkDep "D" "2.0"

      it "Deps A and D are replaced" $
        expectDeps [depAReplaced, depB, depC, depDReplaced] graph
      it "Dep D is replaced in the edges" $
        expectEdges [(depB, depC), (depC, depDReplaced)] graph
      it "DepAReplaced is now a direct dependency" $
        expectDirect [depAReplaced, depB] graph

    context "should should remove main module, and produce graphing with minimal version selection (MVS)" $ do
      -- Act
      let graph = buildGraph $ mkModBuildCfg (GoModReplacements Map.empty) ApplyMVS

      -- Assert
      let depA = mkDep "A" "1.2"
      let depB = mkDep "B" "1.2"
      let depC = mkDep "C" "1.4"
      let depD = mkDep "D" "1.2"

      it "Deps A - D are included" $
        expectDeps [depA, depB, depC, depD] graph
      it "Edges B-C and C-D are in the graph" $
        expectEdges [(depB, depC), (depC, depD)] graph
      it "Direct deps A and B are listed" $
        expectDirect [depA, depB] graph

    it "should should remove main module, and produce graphing with all module versioning" $ do
      -- Act
      let graph = buildGraph $ mkModBuildCfg (GoModReplacements Map.empty) NoMVS

      -- Assert
      let depA1_1 = mkDep "A" "1.1"
      let depA1_2 = mkDep "A" "1.2"
      let depB1_2 = mkDep "B" "1.2"
      let depB1_3 = mkDep "B" "1.3"
      let depC1_1 = mkDep "C" "1.1"
      let depC1_2 = mkDep "C" "1.2"
      let depC1_3 = mkDep "C" "1.3"
      let depC1_4 = mkDep "C" "1.4"
      let depD1_1 = mkDep "D" "1.1"
      let depD1_2 = mkDep "D" "1.2"
      let depE1_1 = mkDep "E" "1.1"
      let depF1_1 = mkDep "F" "1.1"

      expectDeps
        [ depA1_1
        , depA1_2
        , depB1_2
        , depB1_3
        , depC1_1
        , depC1_2
        , depC1_3
        , depC1_4
        , depD1_1
        , depD1_2
        , depE1_1
        , depF1_1
        ]
        graph
      expectEdges
        [ -- A
          (depA1_1, depC1_1)
        , (depA1_2, depC1_3)
        , -- B
          (depB1_2, depC1_4)
        , (depB1_3, depE1_1)
        , -- C
          (depC1_1, depD1_1)
        , (depC1_2, depD1_1)
        , (depC1_3, depD1_2)
        , (depC1_4, depD1_2)
        , -- E
          (depE1_1, depF1_1)
        , -- F
          (depF1_1, depE1_1)
        ]
        graph
      expectDirect [depA1_2, depB1_2] graph

specParse :: Spec
specParse = do
  describe "parseGoGraphMod" $ do
    let shouldParseInto = parseMatch parseGoGraphMod

    it "should parse module without version" $ do
      "github.com/Microsoft/go-winio" `shouldParseInto` MainMod "github.com/Microsoft/go-winio"

    it "should parse module with semver version" $ do
      "go-winio@v0.5.0" `shouldParseInto` OtherMod "go-winio" (semver 0 5 0)

    it "should parse module with pseudo semver version" $ do
      "go-winio@v0.0.0-20210619224110-3f7ff695adc6" `shouldParseInto` OtherMod "go-winio" (Pseudo "3f7ff695adc6")

    it "should parse module with non canonical version" $ do
      "go-winio@LATEST" `shouldParseInto` OtherMod "go-winio" (NonCanonical "LATEST")

  describe "go mod graph parser" $ do
    trivialInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.graph.stdout")
    it "parses a trivial example" $ do
      runParser parseGoModGraph "" trivialInput
        `shouldParse` [ (MainMod "main", OtherMod "dep" (semver 0 5 0))
                      , (OtherMod "dep" (semver 0 5 0), OtherMod "depB" (semver 0 6 0))
                      ]
