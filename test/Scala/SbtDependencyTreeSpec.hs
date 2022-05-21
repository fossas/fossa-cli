{-# LANGUAGE QuasiQuotes #-}

module Scala.SbtDependencyTreeSpec (
  spec,
) where

import Data.Foldable (traverse_)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvProduction),
  DepType (MavenType),
  Dependency (Dependency),
  VerConstraint (CEq),
 )
import GraphUtil (expectDep, expectDirect, expectEdge)
import Graphing (Graphing)
import Strategy.Scala.SbtDependencyTree (buildGraph, removeLogPrefixes, sbtTreeParser)
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  expectationFailure,
  it,
 )
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  checkGraph exampleSingleProjectSbtOut singleProjectGraphSpec
  checkGraph exampleMultiProjectSbtOut multiProjectGraphSpec

exampleSingleProjectSbtOut :: Text
exampleSingleProjectSbtOut =
  [r|[info] welcome to sbt 1.5.5 (Temurin Java 1.8.0_312)
[info] loading settings for project default:project_A-build from plugin.sbt ...
[info] loading project definition from /path
[info] loading settings for project default:project_A from build.sbt ...
[info] set current project to default:project_A (in build file:/path)
[info] default:project_A:0.0.0
[info]   +-net.sourceforge.htmlcleaner:htmlcleaner:2.0
[info]   | +-org.jdom:jdom2:2.0.0
[info]   | 
[info]   +-org.scala-lang:scala3-library_3:3.0 [S]
[info]   
[success] Total time: 0 s, completed 19-May-2022 8:17:29 PM
|]

singleProjectGraphSpec :: Graphing Dependency -> Spec
singleProjectGraphSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  let hasDep :: Dependency -> Expectation
      hasDep dep = expectDep dep graph

  describe "buildGraph" $ do
    it "should correctly graph dependencies" $ do
      expectDirect [mkRawDep "default:project_A" "0.0.0"] graph

      hasDep $ mkRawDep "net.sourceforge.htmlcleaner:htmlcleaner" "2.0"
      hasDep $ mkRawDep "org.jdom:jdom2" "2.0.0"
      hasDep $ mkRawDep "org.scala-lang:scala3-library_3" "3.0"

      hasEdge (mkRawDep "default:project_A" "0.0.0") (mkRawDep "net.sourceforge.htmlcleaner:htmlcleaner" "2.0")
      hasEdge (mkRawDep "default:project_A" "0.0.0") (mkRawDep "org.scala-lang:scala3-library_3" "3.0")
      hasEdge (mkRawDep "net.sourceforge.htmlcleaner:htmlcleaner" "2.0") (mkRawDep "org.jdom:jdom2" "2.0.0")

exampleMultiProjectSbtOut :: Text
exampleMultiProjectSbtOut =
  [r|[info] welcome to sbt 1.5.5 (Temurin Java 1.8.0_312)
[info] Loading settings from plugins.sbt ...
[info] Loading project definition from /som-path/project
[info] Loading settings from build.sbt ...
[info] Set current project to sbt-multi-example (in build file:/some-path/)
[info] Formatting 1 Scala source in global:sbt ...
[success] Total time: 1 s, completed 19-May-2022 2:38:08 PM
[info] Updating {file:/some-path/}common...
[info] Updating {file:/some-path/}global...
[info] Done updating.
[info] org.PARENT:PROJECT:1.0-SNAPSHOT [S]
[info] Updating {file:/some-path/}some-project2...
[info] Done updating.
[info] Updating {file:/some-path/}some-project1...
[info] org:PROJECTA:1.0-SNAPSHOT [S]
[info]   +-org:B:1.0
[info]   | +-org:C:1.0
[info]   | +-org:D:1.0
[info]   | 
[info]   +-org:E:1.0
[info]   +-org:F:1.0
[info]     +-org:G:1.0
[info]       +-org:H:1.0
[info]       +-org:I:1.0
[info]       | +-org:J:1.0
[info]       | | +-org:K:1.0
[info]       | | 
[info]       | +-org:L:1.0
[info]       | | +-org:M:1.0
[info]       | | | +-org:N:1.0
[info]       | | | +-org:O:1.0
[info]       | | | 
[info]       | | +-org:P:1.0
[info]       | | +-org:Q:1.0
[info]       | | 
[info]       | +-org:R:1.0
[info]       +-org:S:1.0
[info]     
[info] Done updating.
[info] org:PROJECTB:1.0-SNAPSHOT [S]
[info]   +-org:T:1.0
[info]   | +-org:U:1.0
[info]   | 
[info]   +-org:W:1.0 (evicted by: 2.0)
[info]     
[info] Done updating.
[info] 
[success] Total time: 3 s, completed 19-May-2022 2:38:11 PM
|]

multiProjectGraphSpec :: Graphing Dependency -> Spec
multiProjectGraphSpec graph = do
  let hasEdge :: Dependency -> Dependency -> Expectation
      hasEdge = expectEdge graph

  let hasDep :: Dependency -> Expectation
      hasDep dep = expectDep dep graph

  describe "buildGraph" $ do
    it "should correctly graph dependencies" $ do
      expectDirect [mkRawDep "org.PARENT:PROJECT" "1.0-SNAPSHOT", mkRawDep "org:PROJECTA" "1.0-SNAPSHOT", mkRawDep "org:PROJECTB" "1.0-SNAPSHOT"] graph

      -- For sub project a
      traverse_
        hasDep
        [ mkDep "B@1.0"
        , mkDep "C@1.0"
        , mkDep "D@1.0"
        , mkDep "E@1.0"
        , mkDep "F@1.0"
        , mkDep "G@1.0"
        , mkDep "H@1.0"
        , mkDep "I@1.0"
        , mkDep "J@1.0"
        , mkDep "K@1.0"
        , mkDep "L@1.0"
        , mkDep "M@1.0"
        , mkDep "N@1.0"
        , mkDep "O@1.0"
        , mkDep "P@1.0"
        , mkDep "Q@1.0"
        , mkDep "R@1.0"
        , mkDep "S@1.0"
        ]

      hasEdge (mkDep "PROJECTA@1.0-SNAPSHOT") (mkDep "B@1.0")
      hasEdge (mkDep "PROJECTA@1.0-SNAPSHOT") (mkDep "E@1.0")
      hasEdge (mkDep "PROJECTA@1.0-SNAPSHOT") (mkDep "F@1.0")

      hasEdge (mkDep "B@1.0") (mkDep "C@1.0")
      hasEdge (mkDep "B@1.0") (mkDep "D@1.0")
      hasEdge (mkDep "F@1.0") (mkDep "G@1.0")

      hasEdge (mkDep "G@1.0") (mkDep "H@1.0")
      hasEdge (mkDep "G@1.0") (mkDep "I@1.0")
      hasEdge (mkDep "G@1.0") (mkDep "S@1.0")

      hasEdge (mkDep "I@1.0") (mkDep "J@1.0")
      hasEdge (mkDep "I@1.0") (mkDep "L@1.0")
      hasEdge (mkDep "I@1.0") (mkDep "R@1.0")

      hasEdge (mkDep "J@1.0") (mkDep "K@1.0")

      hasEdge (mkDep "L@1.0") (mkDep "M@1.0")
      hasEdge (mkDep "L@1.0") (mkDep "P@1.0")
      hasEdge (mkDep "L@1.0") (mkDep "Q@1.0")

      hasEdge (mkDep "M@1.0") (mkDep "N@1.0")
      hasEdge (mkDep "M@1.0") (mkDep "O@1.0")

      -- For sub project b
      traverse_
        hasDep
        [ mkDep "T@1.0"
        , mkDep "W@2.0"
        , mkDep "U@1.0"
        ]

      hasEdge (mkDep "PROJECTB@1.0-SNAPSHOT") (mkDep "T@1.0")
      hasEdge (mkDep "PROJECTB@1.0-SNAPSHOT") (mkDep "W@2.0")
      hasEdge (mkDep "T@1.0") (mkDep "U@1.0")

mkRawDep :: Text -> Text -> Dependency
mkRawDep name version =
  Dependency
    MavenType
    name
    (CEq <$> Just version)
    mempty
    (Set.singleton EnvProduction)
    mempty

mkDep :: Text -> Dependency
mkDep nameAtVersion = do
  let nameAndVersionSplit = Text.splitOn "@" nameAtVersion
      name = head nameAndVersionSplit
      version = last nameAndVersionSplit
  mkRawDep ("org:" <> name) version

checkGraph :: Text -> (Graphing Dependency -> Spec) -> Spec
checkGraph rawOutput buildGraphSpec = do
  case runParser sbtTreeParser "" (removeLogPrefixes rawOutput) of
    Left err -> describe "sbtDependencyTree" $ it "should parse sbt output" (expectationFailure $ errorBundlePretty err)
    Right parsedDeps -> buildGraphSpec $ buildGraph parsedDeps
