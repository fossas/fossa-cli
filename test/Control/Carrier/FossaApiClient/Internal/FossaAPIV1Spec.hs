{-# LANGUAGE OverloadedStrings #-}

module Control.Carrier.FossaApiClient.Internal.FossaAPIV1Spec (spec) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 (
  FossaError (..),
  FossaPublicFacingError (..),
  fatalFossaError,
  isApiKeyFailure,
 )
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Diag.Result (ErrGroup (..), ErrHelp (..), Result (..))
import Errata (Errata (errataHeader))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Internal qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- A response carrying nothing but the status code, which is the only field
-- the classification reads.
responseWithStatus :: Int -> HTTP.Response ()
responseWithStatus code =
  HTTP.Response
    { HTTP.responseStatus = HTTP.mkStatus code ""
    , HTTP.responseVersion = HTTP.http11
    , HTTP.responseHeaders = []
    , HTTP.responseBody = ()
    , HTTP.responseCookieJar = mempty
    , HTTP.responseClose' = HTTP.ResponseClose (pure ())
    , HTTP.responseOriginalRequest = HTTP.defaultRequest
    , HTTP.responseEarlyHints = []
    }

statusCodeError :: Int -> FossaError
statusCodeError code = StatusCodeError HTTP.defaultRequest (responseWithStatus code)

-- The headers of every help attached to a failure, so a test can assert both
-- that help was attached and that it is the help we meant.
helpHeaders :: Result a -> [Text]
helpHeaders (Failure _ (ErrGroup _ _ helps _ _ _)) =
  [header | ErrHelp hlp <- helps, Just header <- [errataHeader (renderDiagnostic hlp)]]
helpHeaders Success{} = []

runFatal :: FossaError -> IO (Result ())
runFatal err = runStack . runDiagnostics $ fatalFossaError err

apiKeyHelp :: Text
apiKeyHelp = "Ensure that you are using a valid FOSSA_API_KEY. Refer to the provided documentation for guidance on how to generate and retrieve your API key."

spec :: Spec
spec = do
  describe "isApiKeyFailure" $ do
    it "blames the key only on a 401" $ do
      isApiKeyFailure (statusCodeError 401) `shouldBe` True
      isApiKeyFailure (statusCodeError 500) `shouldBe` False

    it "leaves 403 alone, because we tell the user it is probably a proxy" $
      -- The 403 renderer argues at length that a 403 more likely means a
      -- network appliance intercepted the request than that the key is bad.
      -- Blaming the key here would contradict the message we print there.
      isApiKeyFailure (statusCodeError 403) `shouldBe` False

    it "does not blame the key for problems that are not about the key" $
      -- ANE-3292: an SSO proxy answering 200 with an HTML login page decodes
      -- to this, and the user was told to regenerate a key that was fine.
      mapM_
        (`shouldSatisfy` (not . isApiKeyFailure))
        [ JsonDeserializeError "Unexpected \"<!DOCTYPE html>\", expecting JSON value"
        , ConnectionTimeoutError HTTP.defaultRequest
        , TlsNotSupportedError HTTP.defaultRequest
        , ConnectionClosedError HTTP.defaultRequest
        , ProxyConnectError HTTP.defaultRequest "proxy.invalid" 8080 (HTTP.mkStatus 502 "")
        , -- The status code is not retained on this branch, and the backend's
          -- own message is already the authoritative description.
          BackendPublicFacingError (FossaPublicFacingError "Unauthorized" "some-uuid")
        ]

  describe "fatalFossaError" $ do
    it "attaches the API key help to a 401" $ do
      res <- runFatal (statusCodeError 401)
      helpHeaders res `shouldBe` [apiKeyHelp]

    it "attaches no help to a deserialization failure" $ do
      res <- runFatal (JsonDeserializeError "Unexpected \"<!DOCTYPE html>\", expecting JSON value")
      helpHeaders res `shouldBe` []
