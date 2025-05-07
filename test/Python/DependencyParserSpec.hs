module Python.DependencyParserSpec (spec) where

import Test.Hspec

import Data.Text (Text)
import Strategy.Python.DependencyParser

spec :: Spec
spec = do
  describe "parseDependencySource" $ do
    describe "gitSourceParser" $ do
      it "parses basic git references" $ do
        let input = "git+https://github.com/user/repo.git"
        case parseDependencySource input of
          Right (GitSource url reference) -> do
            url `shouldBe` "https://github.com/user/repo.git"
            reference `shouldBe` Nothing
          _ -> expectationFailure "Failed to parse git dependency"

      it "parses git references with tags" $ do
        let input = "git+https://github.com/user/repo.git@v1.0.0"
        case parseDependencySource input of
          Right (GitSource url reference) -> do
            url `shouldBe` "https://github.com/user/repo.git"
            reference `shouldBe` Just "v1.0.0"
          _ -> expectationFailure "Failed to parse git dependency with tag"

    describe "httpSourceParser" $ do
      it "parses HTTP URLs" $ do
        let input = "http://example.com/package.tar.gz"
        case parseDependencySource input of
          Right (HttpSource url) -> 
            url `shouldBe` "http://example.com/package.tar.gz"
          _ -> expectationFailure "Failed to parse HTTP dependency"

      it "parses HTTPS URLs" $ do
        let input = "https://example.com/package.tar.gz"
        case parseDependencySource input of
          Right (HttpSource url) -> 
            url `shouldBe` "https://example.com/package.tar.gz"
          _ -> expectationFailure "Failed to parse HTTPS dependency"

    describe "fileSourceParser" $ do
      it "parses file: URLs" $ do
        let input = "file:///path/to/package"
        case parseDependencySource input of
          Right (FileSource path) -> 
            path `shouldBe` "/path/to/package"
          _ -> expectationFailure "Failed to parse file dependency"

      it "parses relative paths with ./" $ do
        let input = "./relative/path"
        case parseDependencySource input of
          Right (FileSource path) -> 
            path `shouldBe` "./relative/path"
          _ -> expectationFailure "Failed to parse relative path dependency"

      it "parses relative paths with ../" $ do
        let input = "../parent/path"
        case parseDependencySource input of
          Right (FileSource path) -> 
            path `shouldBe` "../parent/path"
          _ -> expectationFailure "Failed to parse parent path dependency"

      it "parses absolute paths" $ do
        let input = "/absolute/path"
        case parseDependencySource input of
          Right (FileSource path) -> 
            path `shouldBe` "/absolute/path"
          _ -> expectationFailure "Failed to parse absolute path dependency"

    describe "simpleSourceParser" $ do
      it "parses simple version specifications" $ do
        let input = ">=1.0.0"
        case parseDependencySource input of
          Right (SimpleSource ver) -> 
            ver `shouldBe` ">=1.0.0"
          _ -> expectationFailure "Failed to parse simple version dependency"

      it "parses complex version specifications" $ do
        let input = ">=1.0.0,<2.0.0"
        case parseDependencySource input of
          Right (SimpleSource ver) -> 
            ver `shouldBe` ">=1.0.0,<2.0.0"
          _ -> expectationFailure "Failed to parse complex version dependency"