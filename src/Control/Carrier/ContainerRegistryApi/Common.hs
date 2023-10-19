module Control.Carrier.ContainerRegistryApi.Common (
  logHttp,
  fromResponse,
  originalReqUri,
  getHeaderValue,
  getContentType,
  acceptsContentType,
  RegistryCtx (..),
  AuthToken (..),
  getToken,
  updateToken,
) where

import Control.Algebra (Has)
import Control.Carrier.ContainerRegistryApi.Errors (
  ContainerRegistryApiErrorBody,
  UnknownApiError (UnknownApiError),
 )
import Control.Concurrent.STM (STM, TMVar, atomically, putTMVar, tryReadTMVar)
import Control.Effect.Diagnostics (Diagnostics, fatal)
import Control.Effect.Lift (Lift, sendIO)
import Data.Aeson (decode')
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.List (find)
import Data.String.Conversion (ConvertUtf8 (encodeUtf8), decodeUtf8)
import Data.Text (Text)
import Effect.Logger (AnsiStyle, Doc, Logger, Pretty (pretty), logDebug)
import Network.HTTP.Client (
  Manager,
  Request (method, requestHeaders),
  Response (responseBody, responseHeaders, responseStatus),
  getOriginalRequest,
  getUri,
  httpLbs,
 )
import Network.HTTP.Types (ResponseHeaders, hAccept, hContentType, ok200, status400, status404)
import Network.HTTP.Types.Header (HeaderName)
import Network.URI (URI)

-- | Makes request, and logs request uri and responses with debug severity.
logHttp :: (Has (Lift IO) sig m, Has Logger sig m) => Request -> Manager -> m (Response ByteStringLazy.ByteString)
logHttp req manager = do
  logDebug summarizeRequest
  resp <- sendIO $ httpLbs req manager
  logDebug . summarizeResponse $ resp
  pure resp
  where
    summarizeRequest :: Doc AnsiStyle
    summarizeRequest = pretty $ "Requesting: " <> show (method req) <> " " <> show (getUri req)

    summarizeResponse :: Response ByteStringLazy.ByteString -> Doc AnsiStyle
    summarizeResponse r =
      pretty $
        "Received: "
          <> show (responseStatus r)
          <> " (Content-Type:"
          <> show (getContentType . responseHeaders $ r)
          <> ")"
          <> (errResponseBody r)
          <> show (responseHeaders r)

    errResponseBody :: Response ByteStringLazy.ByteString -> String
    errResponseBody r =
      if (responseStatus r == status400 || responseStatus r == status404)
        then " Content: " <> show (responseBody r)
        else ""

-- | Throws Diagnostics Error for Api Errors, and unexpected responses.
fromResponse :: Has Diagnostics sig m => Response ByteStringLazy.ByteString -> m (Response ByteStringLazy.ByteString)
fromResponse resp =
  if (responseStatus resp == ok200)
    then pure resp
    else do
      case decode' (responseBody resp) of
        Just (registryApiErr :: ContainerRegistryApiErrorBody) -> fatal (originalReqUri resp, registryApiErr)
        Nothing -> fatal $ UnknownApiError (originalReqUri resp) (responseStatus resp)

-- | Gets original request uri from response.
originalReqUri :: Response a -> URI
originalReqUri = getUri . getOriginalRequest

-- | Gets Decoded Header Value of Header Name.
getHeaderValue :: HeaderName -> ResponseHeaders -> Maybe Text
getHeaderValue headerName headers = decodeUtf8 . snd <$> find (\(h, _) -> h == headerName) headers

-- | Gets Content Type.
getContentType :: ResponseHeaders -> Maybe Text
getContentType = getHeaderValue hContentType

-- | Applies 'Accepts' header to request.
acceptsContentType :: Request -> Text -> Request
acceptsContentType r contentType = r{requestHeaders = requestHeaders r ++ [(hAccept, encodeUtf8 contentType)]}

-- | Represents authorization token
data AuthToken
  = BearerAuthToken Text
  | BasicAuthToken Text Text
  deriving (Show, Eq, Ord)

-- | Wrapper for context - e.g. access token etc.
newtype RegistryCtx = RegistryCtx
  { registryAccessToken :: TMVar AuthToken
  }

-- | Gets access token from registry context.
getToken :: (Has (Lift IO) sig m) => RegistryCtx -> m (Maybe AuthToken)
getToken = sendSTM . tryReadTMVar . registryAccessToken

-- | Updates access token from registry context.
updateToken :: Has (Lift IO) sig m => RegistryCtx -> AuthToken -> m ()
updateToken token newVal = sendSTM $ putTMVar (registryAccessToken token) newVal

sendSTM :: Has (Lift IO) sig m => STM a -> m a
sendSTM = sendIO . atomically
