{-# LANGUAGE QuasiQuotes #-}

module Swift.Xcode.PbxprojParserSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Strategy.Swift.Xcode.PbxprojParser (
  AsciiValue (..),
  PbxProj (..),
  parseAsciiDict,
  parseAsciiList,
  parseAsciiText,
  parseAsciiValue,
  parsePbxProj,
 )
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  expectationFailure,
  it,
  runIO,
  shouldBe,
  shouldContain,
  shouldNotBe,
 )
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (
  Parsec,
  errorBundlePretty,
  parse,
  runParser,
 )
import Text.RawString.QQ (r)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

simplePbxProjFile :: Text
simplePbxProjFile =
  [r|// !$*UTF8*$!
{
    // some line comment
	archiveVersion = 1;
	classes = {
	};
	objectVersion = 52;
	objects = {
        /* Begin PBXBuildFile section */
		172D94BF26C5D824008A4DB2 /* Vapor in Frameworks */ = {isa = PBXBuildFile; productRef = 172D94BE26C5D824008A4DB2 /* Vapor */; };
    };
    rootObject = 17874CD926C46B8500D16CA9 /* Project object */;
}|]

unSupportedPbxProjFile :: Text
unSupportedPbxProjFile =
  [r|// !$*NOT-UTF8*$!
{
	archiveVersion = 1;
}|]

spec :: Spec
spec = do
  describe "parseAsciiText" $ do
    let shouldParseInto = parseMatch parseAsciiText

    it "should parse text" $ do
      "a" `shouldParseInto` AText "a"
      "ab" `shouldParseInto` AText "ab"
      "ab-c" `shouldParseInto` AText "ab-c"
      "ab-c.d" `shouldParseInto` AText "ab-c.d"

    it "should parse quoted text" $ do
      [r|"ab-c.d e"|] `shouldParseInto` AText [r|ab-c.d e|]
      [r|"\"$(A)/$(B)\""|] `shouldParseInto` AText [r|"$(A)/$(B)"|]
      [r|"$(A)\..\A\B"|] `shouldParseInto` AText [r|$(A)\..\A\B|]
      [r|"exp A=\"${B:=0}\"\necho \"exp C=${D}\" > \"${E}/../.F.env\"\n if [-z \"${Z}\"]; \n"|]
        `shouldParseInto` AText [r|exp A="${B:=0}"\necho "exp C=${D}" > "${E}/../.F.env"\n if [-z "${Z}"]; \n|]

  describe "parseAsciiList" $ do
    let shouldParseInto = parseMatch parseAsciiList
    it "should parse empty list" $ do
      "( )" `shouldParseInto` AList []
      "()" `shouldParseInto` AList []

    it "should parse list of text" $ do
      "( a )" `shouldParseInto` AList [AText "a"]
      "( a, )" `shouldParseInto` AList [AText "a"]
      "( a, b )" `shouldParseInto` AList [AText "a", AText "b"]
      "( a, b, )" `shouldParseInto` AList [AText "a", AText "b"]
      "(\na,\nb,\n)" `shouldParseInto` AList [AText "a", AText "b"]

    it "should parse list of dictionary types" $
      "( { b = c } )" `shouldParseInto` AList [ADict $ Map.fromList [("b", AText "c")]]

    it "should parse list of mixed types" $ do
      "( a, { b = c } )" `shouldParseInto` AList [AText "a", ADict $ Map.fromList [("b", AText "c")]]
      "( a, { b = c }, )" `shouldParseInto` AList [AText "a", ADict $ Map.fromList [("b", AText "c")]]
      "(\na,\n{ b = c }\n)" `shouldParseInto` AList [AText "a", ADict $ Map.fromList [("b", AText "c")]]

  describe "parseAsciiDict" $ do
    let shouldParseInto = parseMatch parseAsciiDict
    it "should parse empty dictionary" $ do
      "{ }" `shouldParseInto` ADict (Map.empty)
      "{}" `shouldParseInto` ADict (Map.empty)

    it "should parse dictionary with key, and value of text" $ do
      "{ b = c }" `shouldParseInto` ADict (Map.fromList [("b", AText "c")])
      "{ b = c; }" `shouldParseInto` ADict (Map.fromList [("b", AText "c")])
      "{ b = c; d = \"e\" }" `shouldParseInto` ADict (Map.fromList [("b", AText "c"), ("d", AText "e")])

    it "should parse dictionary with key, and value of list" $ do
      "{ f = () }" `shouldParseInto` ADict (Map.fromList [("f", AList [])])
      "{ f = (); }" `shouldParseInto` ADict (Map.fromList [("f", AList [])])
      "{ f = ( g ) }" `shouldParseInto` ADict (Map.fromList [("f", AList [AText "g"])])
      "{ f = (g) }" `shouldParseInto` ADict (Map.fromList [("f", AList [AText "g"])])

    it "should parse dictionary with key, and value of dict" $ do
      "{ h = { } }" `shouldParseInto` ADict (Map.fromList [("h", ADict Map.empty)])
      "{ h = { }; }" `shouldParseInto` ADict (Map.fromList [("h", ADict Map.empty)])
      "{ h = { another = dict } }" `shouldParseInto` ADict (Map.fromList [("h", ADict $ Map.fromList [("another", AText "dict")])])

    it "should parse dictionary with multiple keys, and multiple value types" $
      "{ i = j; k = ( l ); m = { n = o } }"
        `shouldParseInto` ADict
          ( Map.fromList
              [ ("i", AText "j")
              , ("k", AList [AText "l"])
              , ("m", ADict $ Map.fromList [("n", AText "o")])
              ]
          )

  describe "parseAsciiValue" $ do
    let shouldParseInto = parseMatch parseAsciiValue
    it "should parse any ascii value type" $ do
      -- Text
      "a" `shouldParseInto` AText "a"

      -- List
      "( )" `shouldParseInto` AList []
      "( a )" `shouldParseInto` AList [AText "a"]
      "( a, )" `shouldParseInto` AList [AText "a"]

      -- Dictionary
      "{ }" `shouldParseInto` ADict (Map.empty)
      "{}" `shouldParseInto` ADict (Map.empty)
      "{ b = c }" `shouldParseInto` ADict (Map.fromList [("b", AText "c")])
      "{ b = c; }" `shouldParseInto` ADict (Map.fromList [("b", AText "c")])
      "{ b = c; d = \"e\" }" `shouldParseInto` ADict (Map.fromList [("b", AText "c"), ("d", AText "e")])

  describe "parsePbxProj" $ do
    pbxprojFile <- runIO (TIO.readFile "test/Swift/Xcode/testdata/project.pbxproj")
    it "should parse pbxproj.project" $
      case runParser parsePbxProj "" pbxprojFile of
        Left _ -> expectationFailure "failed to parse"
        Right result -> do
          archiveVersion result `shouldBe` "1"
          objectVersion result `shouldBe` "52"
          rootObject result `shouldBe` "17874CD926C46B8500D16CA8"
          classes result `shouldBe` Just (ADict Map.empty)
          objects result `shouldNotBe` Nothing

    it "should parse pbxproj.project with just record fields" $
      case runParser parsePbxProj "" simplePbxProjFile of
        Left _ -> expectationFailure "failed to parse"
        Right result -> do
          archiveVersion result `shouldBe` "1"
          objectVersion result `shouldBe` "52"
          rootObject result `shouldBe` "17874CD926C46B8500D16CA9"
          classes result `shouldBe` Just (ADict Map.empty)
          objects result
            `shouldBe` Just
              ( ADict $
                  Map.fromList
                    [
                      ( "172D94BF26C5D824008A4DB2"
                      , ADict $
                          Map.fromList
                            [ ("isa", AText "PBXBuildFile")
                            , ("productRef", AText "172D94BE26C5D824008A4DB2")
                            ]
                      )
                    ]
              )

    it "should fail when provided with non utf-8 encoding" $
      case runParser parsePbxProj "" unSupportedPbxProjFile of
        Left errUnSupportFile -> errorBundlePretty errUnSupportFile `shouldContain` "expecting to have UTF8 Encoding!"
        Right _ -> expectationFailure "should not parse this file!"
