{-# LANGUAGE OverloadedStrings #-}

module Control.Carrier.FossaApiClient.Internal.FossaAPIV1Spec (spec) where

import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 (
  FossaError (..),
  SignInPageResponse (..),
  checkForSignInPage,
  looksLikeSignInPath,
 )
import Data.Maybe (isJust, isNothing)
import Data.String.Conversion (toText)
import Data.Text (Text, isInfixOf)
import Data.Text qualified as Text
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Errata (Errata (..))
import Errata.Types (Body)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Internal qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | A response whose only interesting field is where the redirect chain
-- ended, which is what the detection reads.
responseLandingOn :: HTTP.Request -> Int -> HTTP.Response ()
responseLandingOn landed code =
  HTTP.Response
    { HTTP.responseStatus = HTTP.mkStatus code ""
    , HTTP.responseVersion = HTTP.http11
    , HTTP.responseHeaders = []
    , HTTP.responseBody = ()
    , HTTP.responseCookieJar = mempty
    , HTTP.responseClose' = HTTP.ResponseClose (pure ())
    , HTTP.responseOriginalRequest = landed
    , HTTP.responseEarlyHints = []
    }

-- The two ends of the real ANE-3292 redirect, taken from a live request to
-- app.fossa.com with the Authorization header absent.
apiRequest :: HTTP.Request
apiRequest = HTTP.parseRequest_ "https://app.fossa.com/api/cli/organization"

loginRequest :: HTTP.Request
loginRequest = HTTP.parseRequest_ "https://app.fossa.com/auth/login?next=%2Fapi%2Fcli%2Forganization"

renderedText :: FossaError -> Text
renderedText err = Text.unlines $ catHeader (renderDiagnostic err)
  where
    catHeader :: Errata -> [Text]
    catHeader e = maybe [] pure (errataHeader e) <> foldMap bodyText (errataBody e)

    bodyText :: Body -> [Text]
    bodyText = pure . toText . show

signInError :: FossaError
signInError = SignInPageError $ SignInPageResponse apiRequest loginRequest (HTTP.mkStatus 200 "OK")

spec :: Spec
spec = do
  describe "looksLikeSignInPath" $ do
    it "recognises the path FOSSA actually redirects to" $
      looksLikeSignInPath "/auth/login" `shouldBe` True

    it "recognises the other shapes an appliance might use" $
      mapM_
        (`shouldSatisfy` looksLikeSignInPath)
        ["/login", "/signin", "/sign-in", "/sso/saml", "/AUTH/LOGIN"]

    it "leaves real API paths alone" $
      mapM_
        (`shouldSatisfy` (not . looksLikeSignInPath))
        ["/api/cli/organization", "/api/revisions", "/api/cli/foo/issues"]

  describe "checkForSignInPage" $ do
    it "fires when a JSON call lands on a sign-in page" $
      -- ANE-3292: a proxy strips Authorization, FOSSA 302s to /auth/login,
      -- http-client follows, and aeson fails on <!DOCTYPE html>.
      checkForSignInPage apiRequest (responseLandingOn loginRequest 200)
        `shouldSatisfy` isJust

    it "stays quiet when nothing redirected" $
      -- The ordinary success path, and every ordinary failure: the response
      -- came back from the endpoint we asked for.
      checkForSignInPage apiRequest (responseLandingOn apiRequest 200)
        `shouldSatisfy` isNothing

    it "stays quiet on a redirect that is not a sign-in page" $
      -- Keyed on the destination, not on the fact of redirecting, so a CDN
      -- or signed-URL hop is not mistaken for a missing credential.
      checkForSignInPage
        apiRequest
        (responseLandingOn (HTTP.parseRequest_ "https://cdn.fossa.com/reports/123") 200)
        `shouldSatisfy` isNothing

  describe "the sign-in page message" $ do
    it "names where the request actually landed" $
      renderedText signInError `shouldSatisfy` isInfixOf "auth/login"

    it "rules the API key out rather than adding it to a checklist" $ do
      -- The whole point of ANE-3292 is removing the false lead. If this
      -- message ever starts hedging about the key, the bug is back.
      renderedText signInError `shouldSatisfy` isInfixOf "Your API key is probably fine"
      renderedText signInError `shouldSatisfy` (not . isInfixOf "valid FOSSA_API_KEY")

    it "points at the one thing worth checking" $
      renderedText signInError `shouldSatisfy` isInfixOf "Authorization header"

  describe "errors that are not about the API key" $
    it "never tells the user to regenerate their key" $
      -- These all used to inherit "ensure you are using a valid
      -- FOSSA_API_KEY" from the blanket errHelp in PreflightChecks.
      --
      -- Each is checked for non-empty output first, so a renderer that
      -- produced nothing at all could not pass this by saying nothing.
      mapM_
        ( \e -> do
            renderedText e `shouldSatisfy` (not . Text.null . Text.strip)
            renderedText e `shouldSatisfy` (not . isInfixOf "FOSSA_API_KEY")
        )
        [ JsonDeserializeError "Unexpected \"<!DOCTYPE html>\", expecting JSON value"
        , ConnectionTimeoutError apiRequest
        , ResponseTimeoutError apiRequest
        , TlsNotSupportedError apiRequest
        , ConnectionClosedError apiRequest
        , ProxyConnectError apiRequest "proxy.invalid" 8080 (HTTP.mkStatus 502 "")
        ]
