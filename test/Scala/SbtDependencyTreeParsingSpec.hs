{-# LANGUAGE QuasiQuotes #-}

module Scala.SbtDependencyTreeParsingSpec (
  spec,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Strategy.Scala.SbtDependencyTree (
  SbtArtifact (SbtArtifact),
  SbtDep (SbtDep),
  parseEviction,
  parseSbtArtifact,
  sbtTreeParser,
 )
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  it,
 )
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (
  Parsec,
  parse,
 )
import Text.RawString.QQ (r)

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

  describe "parseEviction" $ do
    let shouldParseInto = parseMatch parseEviction
    it "should parse sbt dependency eviction" $ do
      "(evicted by: 1.0)" `shouldParseInto` "1.0"
      "(evicted by: 1.0.0)" `shouldParseInto` "1.0.0"
      "(evicted by: 1.0.0-SNAPSHOT)" `shouldParseInto` "1.0.0-SNAPSHOT"

  parsesTreeSpec

treeWithNoDep :: Text
treeWithNoDep =
  [r|
org:project:1.0.0-snapshot
|]

treeWithOneDep :: Text
treeWithOneDep =
  [r|
org:project:1.0.0-snapshot
  +-org:childA:1.0
|]

treeWithOneDepWithEvication :: Text
treeWithOneDepWithEvication =
  [r|
org:project:1.0.0-snapshot
  +-org:childA:1.0 (evicted by: 2.0)
|]

treeWithNestedDep :: Text
treeWithNestedDep =
  [r|
org:project:1.0.0-snapshot
  +-org:childA:1.0
    +-org:grandChildA:2.0
|]

treeWithMultipleNestedDep :: Text
treeWithMultipleNestedDep =
  [r|
org:project:1.0.0-snapshot
  +-org:childA:1.0
  +-org:childB:2.0
  | +-org:grandChildB:3.0
  | 
  +-org:childD:4.0
|]

slightlyComplexTree :: Text
slightlyComplexTree =
  [r|
org:project:1.0.0-snapshot
  +-org:childA:1
  +-org:childB:1
  | +-org:grandChildB:1
  | | +-org:greatGrandChildB:1
  | | | +-org:greatGreatGrandChildB:1
  | | +-org:greatGrandChildC:1
  | +-org:grandChildD:1
  +-org:childE:1
|]

mkArtifact :: Text -> SbtArtifact
mkArtifact nameAtVersion = do
  let nameAndVersionSplit = Text.splitOn "@" nameAtVersion
      artifactId = head nameAndVersionSplit
      version = last nameAndVersionSplit
  SbtArtifact "org" artifactId version

parsesTreeSpec :: Spec
parsesTreeSpec = do
  describe "sbtTreeParser" $ do
    let shouldParseInto = parseMatch sbtTreeParser

    it "should parse tree with no dep" $
      treeWithNoDep
        `shouldParseInto` [SbtDep (mkArtifact "project@1.0.0-snapshot") mempty]

    it "should parse tree with one dep" $
      treeWithOneDep
        `shouldParseInto` [ SbtDep
                              (mkArtifact "project@1.0.0-snapshot")
                              [ SbtDep (mkArtifact "childA@1.0") []
                              ]
                          ]

    it "should parse tree with one dep" $
      treeWithOneDepWithEvication
        `shouldParseInto` [ SbtDep
                              (mkArtifact "project@1.0.0-snapshot")
                              [ SbtDep (mkArtifact "childA@2.0") []
                              ]
                          ]

    it "should parse tree with nested dep" $
      treeWithNestedDep
        `shouldParseInto` [ SbtDep
                              (mkArtifact "project@1.0.0-snapshot")
                              [ SbtDep
                                  (mkArtifact "childA@1.0")
                                  [ SbtDep (mkArtifact "grandChildA@2.0") mempty
                                  ]
                              ]
                          ]

    it "should parse tree with multiple nested dep" $
      treeWithMultipleNestedDep
        `shouldParseInto` [ SbtDep
                              (mkArtifact "project@1.0.0-snapshot")
                              [ SbtDep (mkArtifact "childA@1.0") mempty
                              , SbtDep
                                  (mkArtifact "childB@2.0")
                                  [ SbtDep (mkArtifact "grandChildB@3.0") mempty
                                  ]
                              , SbtDep (mkArtifact "childD@4.0") mempty
                              ]
                          ]

    it "should parse tree with slightly complex nested deps" $
      slightlyComplexTree
        `shouldParseInto` [ SbtDep
                              (mkArtifact "project@1.0.0-snapshot")
                              [ SbtDep (mkArtifact "childA@1") mempty
                              , SbtDep
                                  (mkArtifact "childB@1")
                                  [ SbtDep
                                      (mkArtifact "grandChildB@1")
                                      [ SbtDep
                                          (mkArtifact "greatGrandChildB@1")
                                          [ SbtDep (mkArtifact "greatGreatGrandChildB@1") mempty
                                          ]
                                      , SbtDep
                                          (mkArtifact "greatGrandChildC@1")
                                          mempty
                                      ]
                                  , SbtDep (mkArtifact "grandChildD@1") mempty
                                  ]
                              , SbtDep (mkArtifact "childE@1") mempty
                              ]
                          ]
