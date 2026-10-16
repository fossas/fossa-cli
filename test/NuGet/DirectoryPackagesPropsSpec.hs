module NuGet.DirectoryPackagesPropsSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Parse.XML (parseXML, xmlErrorPretty)
import Strategy.NuGet.DirectoryPackagesProps (Segment (..), buildVersionMap, expandProperties, resolveVersions, valueParser)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe, shouldMatchList)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  propsFile <- runIO (TIO.readFile "test/NuGet/testdata/Directory.Packages.props")
  tolerantPropsFile <- runIO (TIO.readFile "test/NuGet/testdata/Directory.Packages.tolerant.props")
  propertiesPropsFile <- runIO (TIO.readFile "test/NuGet/testdata/Directory.Packages.properties.props")

  describe "Directory.Packages.props parser" $ do
    it "parses PackageVersion entries" $ do
      case parseXML propsFile of
        Right props -> do
          let versions = buildVersionMap props
          Map.lookup "one" versions `shouldBe` Just "1.0.0"
          Map.lookup "two" versions `shouldBe` Just "2.0.0"
          Map.lookup "three" versions `shouldBe` Just "3.0.0"
          Map.lookup "four" versions `shouldBe` Just "4.0.0"
          Map.lookup "five" versions `shouldBe` Just "5.0.0"
          -- Keys are case-folded for case-insensitive NuGet package ID matching
          Map.lookup "mixedcase.package" versions `shouldBe` Just "6.0.0"
          Map.lookup "MixedCase.Package" versions `shouldBe` Nothing
          Map.lookup "nonexistent" versions `shouldBe` Nothing
        Left err -> expectationFailure (toString ("could not parse Directory.Packages.props: " <> xmlErrorPretty err))

    it "tolerates PackageVersion entries without a Version attribute" $ do
      case parseXML tolerantPropsFile of
        Right props -> do
          let versions = buildVersionMap props
          Map.lookup "normal" versions `shouldBe` Just "1.0.0"
          -- Version metadata declared as a child element instead of an attribute
          Map.lookup "child.version" versions `shouldBe` Just "2.0.0"
          Map.lookup "updated" versions `shouldBe` Just "3.0.0"
          -- Entries with no resolvable name/version are skipped, not fatal
          Map.lookup "removed" versions `shouldBe` Nothing
          Map.lookup "attrless" versions `shouldBe` Nothing
        Left err -> expectationFailure (toString ("could not parse Directory.Packages.tolerant.props: " <> xmlErrorPretty err))

    it "expands MSBuild property references in versions" $ do
      case parseXML propertiesPropsFile of
        Right props -> do
          let (versions, unresolved) = resolveVersions props
          Map.lookup "literal" versions `shouldBe` Just "1.2.3"
          Map.lookup "simple" versions `shouldBe` Just "10.0.9"
          -- MSBuild property names are case-insensitive
          Map.lookup "caseinsensitive" versions `shouldBe` Just "10.0.9"
          -- A property's value may itself reference another property
          Map.lookup "nested" versions `shouldBe` Just "4.3.0"
          Map.lookup "composed" versions `shouldBe` Just "4.10.0.9-preview"
          -- A later PropertyGroup overrides an earlier definition
          Map.lookup "latergroupwins" versions `shouldBe` Just "2.0.0"
          Map.lookup "childelement" versions `shouldBe` Just "10.0.9"
          -- Unresolvable entries are left out of the map and reported
          Map.lookup "undefined" versions `shouldBe` Nothing
          Map.lookup "cyclic" versions `shouldBe` Nothing
          Map.lookup "propertyfunction" versions `shouldBe` Nothing
          Map.lookup "unterminated" versions `shouldBe` Nothing
          unresolved
            `shouldMatchList` [ ("Undefined", "$(NoSuchProperty)")
                              , ("Cyclic", "$(LoopA)")
                              , ("PropertyFunction", "$(TheVersion.Trim())")
                              , ("Unterminated", "$(TheVersion")
                              ]
        Left err -> expectationFailure (toString ("could not parse Directory.Packages.properties.props: " <> xmlErrorPretty err))

  describe "valueParser" $ do
    it "parses a value without references as a single literal" $
      "1.0.0" `shouldParseInto` [Literal "1.0.0"]

    it "parses an empty value" $
      "" `shouldParseInto` []

    it "parses references mixed with literal text" $
      "v$(Major).$(Minor)-rc" `shouldParseInto` [Literal "v", Ref "Major", Literal ".", Ref "Minor", Literal "-rc"]

    it "treats a dollar sign that does not open a reference as literal" $
      "1$2$" `shouldParseInto` [Literal "1", Literal "$", Literal "2", Literal "$"]

    it "rejects an empty reference" $
      parse valueParser "" `shouldFailOn` ("$()" :: Text)

    it "rejects an unterminated reference" $
      parse valueParser "" `shouldFailOn` ("$(Major" :: Text)

    it "rejects property-function syntax" $ do
      parse valueParser "" `shouldFailOn` ("$(Major.Trim())" :: Text)
      parse valueParser "" `shouldFailOn` ("$([MSBuild]::Add(1, 2))" :: Text)

  describe "expandProperties" $ do
    let props = Map.fromList [("major", "1"), ("minor", "$(Major).2"), ("empty", "")]

    it "leaves values without references untouched" $
      expandProperties props "1.0.0" `shouldBe` Just "1.0.0"

    it "expands references anywhere in the value" $
      expandProperties props "v$(Major)-$(minor)+$(MAJOR)" `shouldBe` Just "v1-1.2+1"

    it "expands to the empty string for empty properties" $
      expandProperties props "$(Empty)" `shouldBe` Just ""

    it "fails on an undefined property" $
      expandProperties props "$(Patch)" `shouldBe` Nothing

    it "fails on an empty reference" $
      expandProperties props "$()" `shouldBe` Nothing

    it "fails on a reference with no closing paren" $
      expandProperties props "$(Major" `shouldBe` Nothing

    it "fails on property-function syntax" $ do
      expandProperties props "$(Major.Trim())" `shouldBe` Nothing
      expandProperties props "$([MSBuild]::Add(1, 2))" `shouldBe` Nothing

shouldParseInto :: Text -> [Segment] -> Expectation
shouldParseInto input expected = parse valueParser "" input `shouldParse` expected
