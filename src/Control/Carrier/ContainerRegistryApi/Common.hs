module Control.Carrier.ContainerRegistryApi.Common (
  logHttp,
  http,
  fromResponse,
  originalReqUri,
  getHeaderValue,
  getContentType,
  acceptsContentType,
) where

import Control.Algebra (Has)
import Control.Carrier.ContainerRegistryApi.Errors (
  ContainerRegistryApiErrorBody,
  UnknownApiError (UnknownApiError),
 )
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
import Network.HTTP.Types (ResponseHeaders, hAccept, hContentType, ok200)
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

    summarizeResponse :: Response a -> Doc AnsiStyle
    summarizeResponse r =
      pretty $
        "Received: "
          <> show (responseStatus r)
          <> " (Content-Type:"
          <> show (getContentType . responseHeaders $ r)
          <> ")"

http :: Has (Lift IO) sig m => Request -> Manager -> m (Response ByteStringLazy.ByteString)
http req manager = sendIO $ httpLbs req manager

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
