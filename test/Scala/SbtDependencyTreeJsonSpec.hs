{-# LANGUAGE TemplateHaskell #-}

module Scala.SbtDependencyTreeJsonSpec (
  spec,
) where

import Data.Text (Text)
import Data.Void (Void)
import DepTypes (
  DepType (MavenType),
  Dependency (Dependency),
  VerConstraint (CEq),
 )
import GraphUtil (expectDeps', expectDirect', expectEdges')
import Path (mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Scala.Common (SbtArtifact (SbtArtifact))
import Strategy.Scala.SbtDependencyTreeJson (analyze, parseSbtArtifact)
import Test.Effect (it')
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  it,
  runIO,
 )
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (
  Parsec,
  parse,
 )

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

spec :: Spec
spec = do
  describe "parseValidProjectIdentifier" $ do
    let shouldParseInto = parseMatch parseSbtArtifact

    it "should parse sbt artifact" $ do
      "a:b:c" `shouldParseInto` SbtArtifact "a" "b" "c"
      "a.b:c:d" `shouldParseInto` SbtArtifact "a.b" "c" "d"
      "a.b-c:d:e" `shouldParseInto` SbtArtifact "a.b-c" "d" "e"
      "a:b-c:d" `shouldParseInto` SbtArtifact "a" "b-c" "d"
      "a:b.c:d" `shouldParseInto` SbtArtifact "a" "b.c" "d"
      "a:b.c:1.0" `shouldParseInto` SbtArtifact "a" "b.c" "1.0"
      "a:b.c:1.0.0" `shouldParseInto` SbtArtifact "a" "b.c" "1.0.0"
      "a:b.c:1.0.0-SNAPSHOT" `shouldParseInto` SbtArtifact "a" "b.c" "1.0.0-SNAPSHOT"
      "a:b.c:1.0 (evicted by 2.0)" `shouldParseInto` SbtArtifact "a" "b.c" "2.0"
      "a:b.c:1.0.0 (cycle)" `shouldParseInto` SbtArtifact "a" "b.c" "1.0.0"
      "a:b.c:1.0.0 (error: some error)" `shouldParseInto` SbtArtifact "a" "b.c" "1.0.0"
      "org.agrona:agrona:[1.16.0,2.0)" `shouldParseInto` SbtArtifact "org.agrona" "agrona" "[1.16.0,2.0)"
      "org.agrona:agrona:[1.16.0,2.0) (evicted by 1.16.0)" `shouldParseInto` SbtArtifact "org.agrona" "agrona" "1.16.0"
      "org.scala-lang.modules:scala-parser-combinators_2.13:1.1.2" `shouldParseInto` SbtArtifact "org.scala-lang.modules" "scala-parser-combinators_2.13" "1.1.2"

  describe "analyze" $ do
    currentDir <- runIO getCurrentDir
    let treeJson = currentDir </> $(mkRelFile "test/Scala/testdata/tree.json")

    it' "should graph from tree json" $ do
      graph <- analyze treeJson
      expectDirect' [scalaLib, akkaStream, logbackEncoder] graph
      expectDeps'
        [ akkaStream
        , akkaActor
        , akkaProtoBuf
        , configCore
        , reactiveStream
        , typeSafeConfig
        , java8Compat_3
        , logbackEncoder
        , jacksonDataBind
        , jacksonAnnotations
        , jacksonCore
        , scalaLib
        , combinators_2
        ]
        graph

      expectEdges'
        [ (akkaStream, akkaActor)
        , (akkaStream, akkaProtoBuf)
        , (akkaStream, configCore)
        , (akkaStream, reactiveStream)
        , (akkaStream, scalaLib)
        , (akkaActor, typeSafeConfig)
        , (akkaActor, java8Compat_3)
        , (java8Compat_3, scalaLib)
        , (akkaActor, scalaLib)
        , (configCore, typeSafeConfig)
        , (configCore, combinators_2)
        , (logbackEncoder, jacksonDataBind)
        , (jacksonDataBind, jacksonAnnotations)
        , (jacksonDataBind, jacksonCore)
        ]
        graph

scalaLib :: Dependency
scalaLib = mkDep "org.scala-lang:scala3-library_3" "3.1.1"

akkaStream :: Dependency
akkaStream = mkDep "com.typesafe.akka:akka-stream_3" "2.6.19"

logbackEncoder :: Dependency
logbackEncoder = mkDep "net.logstash.logback:logstash-logback-encoder" "7.2"

akkaActor :: Dependency
akkaActor = mkDep "com.typesafe.akka:akka-actor_3" "2.6.19"

akkaProtoBuf :: Dependency
akkaProtoBuf = mkDep "com.typesafe.akka:akka-protobuf-v3_3" "2.6.19"

reactiveStream :: Dependency
reactiveStream = mkDep "org.reactivestreams:reactive-streams" "1.0.3"

configCore :: Dependency
configCore = mkDep "com.typesafe:ssl-config-core_2.13" "0.4.3"

typeSafeConfig :: Dependency
typeSafeConfig = mkDep "com.typesafe:config" "1.4.2"

java8Compat_3 :: Dependency
java8Compat_3 = mkDep "org.scala-lang.modules:scala-java8-compat_3" "1.0.0"

combinators_2 :: Dependency
combinators_2 = mkDep "org.scala-lang.modules:scala-parser-combinators_2.13" "1.1.2"

jacksonDataBind :: Dependency
jacksonDataBind = mkDep "com.fasterxml.jackson.core:jackson-databind" "2.13.3"

jacksonAnnotations :: Dependency
jacksonAnnotations = mkDep "com.fasterxml.jackson.core:jackson-annotations" "2.13.3"

jacksonCore :: Dependency
jacksonCore = mkDep "com.fasterxml.jackson.core:jackson-core" "2.13.3"

mkDep :: Text -> Text -> Dependency
mkDep name version =
  Dependency
    MavenType
    name
    (Just $ CEq version)
    mempty
    mempty
    mempty
