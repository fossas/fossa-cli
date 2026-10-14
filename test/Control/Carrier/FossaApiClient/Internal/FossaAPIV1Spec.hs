{-# LANGUAGE OverloadedStrings #-}

module Control.Carrier.FossaApiClient.Internal.FossaAPIV1Spec (spec) where

import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 (
  FossaError (..),
  FossaPublicFacingError (..),
  SignInPageResponse (..),
  checkForSignInPage,
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
import Test.Hspec (Spec, describe, it, shouldSatisfy)

-- | A 200 response whose redirect chain ended at the given URL.
responseLandingOn :: HTTP.Request -> HTTP.Response ()
responseLandingOn landed =
  HTTP.Response
    { HTTP.responseStatus = HTTP.mkStatus 200 ""
    , HTTP.responseVersion = HTTP.http11
    , HTTP.responseHeaders = []
    , HTTP.responseBody = ()
    , HTTP.responseCookieJar = mempty
    , HTTP.responseClose' = HTTP.ResponseClose (pure ())
    , HTTP.responseOriginalRequest = landed
    , HTTP.responseEarlyHints = []
    }

-- What app.fossa.com does when no Authorization header arrives (ANE-3292).
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

backendError :: Int -> FossaError
backendError code =
  BackendPublicFacingError
    (HTTP.mkStatus code "")
    (FossaPublicFacingError "API token is invalid" "a1b2c3")

spec :: Spec
spec = do
  describe "checkForSignInPage" $ do
    it "fires when an API call lands on the sign-in page" $
      checkForSignInPage apiRequest (responseLandingOn loginRequest) `shouldSatisfy` isJust

    it "stays quiet when nothing redirected" $
      checkForSignInPage apiRequest (responseLandingOn apiRequest) `shouldSatisfy` isNothing

  describe "the sign-in page message" $
    it "names the landing URL and rules the API key out" $ do
      let msg = renderedText (SignInPageError $ SignInPageResponse apiRequest loginRequest)
      msg `shouldSatisfy` isInfixOf "auth/login"
      msg `shouldSatisfy` isInfixOf "The supplied API key is likely fine"
      msg `shouldSatisfy` (not . isInfixOf "FOSSA_API_KEY")

  describe "a decoded backend error" $ do
    it "attaches the key help and docs on a 401" $ do
      renderedText (backendError 401) `shouldSatisfy` isInfixOf "FOSSA_API_KEY"
      renderedText (backendError 401) `shouldSatisfy` isInfixOf "generating-an-api-key"

    it "attaches neither on any other status" $ do
      -- So a blank render can't pass.
      renderedText (backendError 500) `shouldSatisfy` isInfixOf "API token is invalid"
      renderedText (backendError 500) `shouldSatisfy` (not . isInfixOf "FOSSA_API_KEY")
      renderedText (backendError 500) `shouldSatisfy` (not . isInfixOf "generating-an-api-key")
