module NuGet.DirectoryPackagesPropsSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text.IO qualified as TIO
import Parse.XML (parseXML, xmlErrorPretty)
import Strategy.NuGet.DirectoryPackagesProps (buildVersionMap)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  propsFile <- runIO (TIO.readFile "test/NuGet/testdata/Directory.Packages.props")
  tolerantPropsFile <- runIO (TIO.readFile "test/NuGet/testdata/Directory.Packages.tolerant.props")

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
