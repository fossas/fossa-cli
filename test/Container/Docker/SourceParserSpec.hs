module Container.Docker.SourceParserSpec (spec) where

import Container.Docker.SourceParser (
  RegistryHostScheme (RegistryHTTP),
  RegistryImageSource (..),
  defaultHttpScheme,
  defaultTag,
  dockerHubRegistry,
  parseImageUrl,
 )
import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec (Expectation, Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (
  Parsec,
  parse,
 )

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser mempty input `shouldParse` expected

spec :: Spec
spec = do
  describe "docker pull text parser" $ do
    let shouldParseInto = parseMatch (parseImageUrl fixtureArch)

    it "should parse repo and repo tag" $ do
      "fossa/nginx" `shouldParseInto` defaultedRegistrySrc "fossa/nginx" defaultTag
      "fossa/nginx:11" `shouldParseInto` defaultedRegistrySrc "fossa/nginx" "11"

    it "should parse repo and repo tag digest" $ do
      "httpd@sha256:abc" `shouldParseInto` defaultedRegistrySrc "library/httpd" "sha256:abc"

    it "should parse registry url, with repo and tag" $ do
      "ghcr.io/fossas/haskell-dev-tools:9.0.2" `shouldParseInto` (RegistryImageSource "ghcr.io" defaultHttpScheme Nothing "fossas/haskell-dev-tools" "9.0.2" fixtureArch)
      "ghcr.io/fossas/haskell-dev-tools" `shouldParseInto` (RegistryImageSource "ghcr.io" defaultHttpScheme Nothing "fossas/haskell-dev-tools" defaultTag fixtureArch)
      "ghcr.io/fossas/haskell-dev-tools@sha256:abc" `shouldParseInto` (RegistryImageSource "ghcr.io" defaultHttpScheme Nothing "fossas/haskell-dev-tools" "sha256:abc" fixtureArch)
      "https://ghcr.io/fossas/haskell-dev-tools:9.0.2" `shouldParseInto` (RegistryImageSource "ghcr.io" defaultHttpScheme Nothing "fossas/haskell-dev-tools" "9.0.2" fixtureArch)
      "http://localhost:3000/fossas/haskell-dev-tools:9.0.2" `shouldParseInto` (RegistryImageSource "localhost:3000" RegistryHTTP Nothing "fossas/haskell-dev-tools" "9.0.2" fixtureArch)

    it "should parse registry url and credentials" $ do
      "user:pass@ghcr.io/fossas/haskell-dev-tools:9.0.2" `shouldParseInto` (RegistryImageSource "ghcr.io" defaultHttpScheme (Just ("user", "pass")) "fossas/haskell-dev-tools" "9.0.2" fixtureArch)
      "https://user:pass@ghcr.io/fossas/haskell-dev-tools:9.0.2" `shouldParseInto` (RegistryImageSource "ghcr.io" defaultHttpScheme (Just ("user", "pass")) "fossas/haskell-dev-tools" "9.0.2" fixtureArch)

fixtureArch :: Text
fixtureArch = "amd64"

defaultedRegistrySrc :: Text -> Text -> RegistryImageSource
defaultedRegistrySrc repo ref =
  RegistryImageSource
    dockerHubRegistry
    defaultHttpScheme
    Nothing
    repo
    ref
    fixtureArch
