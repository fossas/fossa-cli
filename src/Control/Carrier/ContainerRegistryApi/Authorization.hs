module Control.Carrier.ContainerRegistryApi.Authorization (
  mkRequest,
  getAuthToken,
  applyAuthToken,

  -- * for testing
  RegistryAuthChallenge (..),
  RegistryBearerChallenge (..),
  parseAuthChallenge,
) where

import Control.Algebra (Has)
import Control.Carrier.ContainerRegistryApi.Common (
  AuthToken (BasicAuthToken, BearerAuthToken),
  RegistryCtx,
  acceptsContentType,
  fromResponse,
  getHeaderValue,
  getToken,
  logHttp,
  originalReqUri,
  updateToken,
 )
import Control.Carrier.ContainerRegistryApi.Errors (
  ContainerRegistryApiErrorBody,
  FailedToParseAuthChallenge (FailedToParseAuthChallenge),
  UnknownApiError (UnknownApiError),
 )
import Control.Effect.Diagnostics (Diagnostics, fatal, fatalText, fromMaybeText)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (Reader, ask)
import Data.Aeson (FromJSON (parseJSON), decode', eitherDecode, withObject, (.:))
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (ConvertUtf8 (decodeUtf8), encodeUtf8, toString, toText)
import Data.Text (Text, isInfixOf)
import Data.Text qualified as Text
import Data.Void (Void)
import Effect.Logger (Logger)
import Network.HTTP.Client (
  Manager,
  Request (host, method, shouldStripHeaderOnRedirect),
  Response (responseBody, responseHeaders, responseStatus),
  applyBasicAuth,
  applyBearerAuth,
  parseRequest,
 )
import Network.HTTP.Types (methodGet, methodHead, statusCode)
import Network.HTTP.Types.Header (
  hAuthorization,
  hWWWAuthenticate,
 )
import Text.Megaparsec (
  MonadParsec (takeWhileP),
  Parsec,
  between,
  chunk,
  errorBundlePretty,
  parse,
  sepBy,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char)

type Parser = Parsec Void Text

-- | Request with registry authorization middleware.
mkRequest ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader RegistryCtx) sig m
  ) =>
  Manager -> -- Request Manager to use
  Maybe (Text, Text) -> -- Credentials to use when retrieving authorization token
  Maybe [Text] -> -- Content-Type to request
  Request -> -- Request to make
  m (Response ByteStringLazy.ByteString)
mkRequest manager registryCred accepts req = do
  token <- getToken =<< ask
  token' <- getAuthToken registryCred req manager token
  logHttp (applyContentType accepts $ applyAuthToken token' req) manager
  where
    applyContentType :: Maybe [Text] -> Request -> Request
    applyContentType c r = case c of
      Nothing -> req
      Just c' -> r `acceptsContentType` (Text.intercalate ", " c')

-- | Adds 'Authorization' header if token is provided otherwise id
applyAuthToken :: Maybe AuthToken -> Request -> Request
applyAuthToken Nothing r = r
applyAuthToken (Just (BasicAuthToken user pass)) r =
  stripAuthHeaderOnRedirect $ applyBasicAuth (encodeUtf8 user) (encodeUtf8 pass) r
applyAuthToken (Just (BearerAuthToken token)) r =
  stripAuthHeaderOnRedirect $ applyBearerAuth (encodeUtf8 token) r

-- If we don't strip auth headers, on redirect, depending on how
-- blobs/manifest are retrieved cloud vendor may throw 'Bad Request' error.
stripAuthHeaderOnRedirect :: Request -> Request
stripAuthHeaderOnRedirect r =
  if ((isAwsECR || isAzure) && (method r == methodGet || method r == methodHead))
    then r{shouldStripHeaderOnRedirect = (== hAuthorization)}
    else r
  where
    isAwsECR :: Bool
    isAwsECR = "amazonaws.com" `isInfixOf` decodeUtf8 (host r)

    isAzure :: Bool
    isAzure = "azurecr.io" `isInfixOf` decodeUtf8 (host r)

-- | Generates Auth Token For Request.
--
-- Refer to:
--
--  https://docs.docker.com/registry/spec/auth/token/
--
-- OCI Registry SPEC do not explicitly specify auth workflow, but all registries tested follow
-- specified workflow, same as docker registry.
--
-- In Summary, Auth Workflow is:
--
--  1. Client attempts request to desired endpoint (with HEAD)
--  2. If authorization is required, server will respond with 401, and auth challenge.
--
--      Registry should respond with RFC 6750: OAuth 2.0 Authorization Framework: Bearer Token
--      Challenge. This means that, 401 response header of 'WWW-Authenticate',
--      will provide url, scope and service for authorization request.
--
--      Reference: https://tools.ietf.org/html/rfc6750#section-3
--
--  3. Make Request to retrieve Authorization Token (use HTTP AUTH, if private repository).
--  4. Retrieve Authorization Token from (3)
getAuthToken ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader RegistryCtx) sig m
  ) =>
  Maybe (Text, Text) ->
  -- | Username and Password to user when retrieving authorization token
  Request ->
  -- | Request For which to Get Authorization Token
  Manager ->
  -- | Manager to use for requests
  Maybe AuthToken ->
  -- | Existing Token (if any)
  m (Maybe AuthToken)
getAuthToken cred reqAttempt manager token = do
  let request' = applyAuthForExistingToken $ reqAttempt{method = "HEAD"}
  response <- logHttp request' manager

  case (decode' $ responseBody response, statusCode . responseStatus $ response) of
    -- If Registry does not have auth challenge, we will see successful response.
    -- meaning that our token is valid, or we do not require authorization token.
    (Nothing, 200) -> pure token
    (_, 401) -> do
      case parse parseAuthChallenge "" <$> getHeaderValue hWWWAuthenticate (responseHeaders response) of
        -- -
        -- Did not receive valid auth challenge
        -- -
        Nothing ->
          fatalText
            ( "Registry did not provide expected 'WWW-Authenticate' challenge: "
                <> (toText . show $ originalReqUri response)
            )
        -- -
        -- Failed to Parse Auth Challenge
        -- -
        Just (Left err) -> fatalText . toText $ errorBundlePretty err
        -- -
        -- Retrieve fresh authorization token, and update in current
        -- registry context.
        -- -
        Just (Right authChallenge) -> do
          token' <- getTokenFromAuthChallenge cred authChallenge manager
          ctx <- ask
          updateToken ctx token'
          pure (Just token')

    -- -
    -- Other Errors
    -- -
    (Just (apiErrors :: ContainerRegistryApiErrorBody), _) -> fatal (originalReqUri response, apiErrors)
    (Nothing, _) -> fatal $ UnknownApiError (originalReqUri response) $ responseStatus response
  where
    applyAuthForExistingToken :: Request -> Request
    applyAuthForExistingToken = applyAuthToken token

-- | Retrieves Token from Authorization Server.
--
-- Use Token provided in AuthChallenge, to request authorization token.
--
-- We make request to Auth Bearer, with required scope and service. It is expected
-- that server will respond with Token. From experience, in some implementation of
-- registry explicit expiration is provided, while in some it is omitted. For example,
-- Github will provide token without expiration, while default Docker index will provide
-- expiration. By default, assumption is that token will expire in 60 seconds. Which is
-- plenty of time to make authorized request.
getTokenFromAuthChallenge ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Maybe (Text, Text) ->
  -- | Username and Password to user when retrieving authorization token
  RegistryAuthChallenge ->
  -- | Authorization Challenge
  Manager ->
  -- | Request Manager to use for subsequent requests
  m AuthToken
getTokenFromAuthChallenge cred (BearerAuthChallenge (RegistryBearerChallenge url service scope)) manager = do
  req <- authTokenEndpoint

  -- If there are auth provided for private registries
  -- use them, when requesting access token
  let req' = case cred of
        Nothing -> req
        Just (user, pass) -> applyBasicAuth (encodeUtf8 user) (encodeUtf8 pass) req

  response <- fromResponse =<< logHttp req' manager
  case eitherDecode $ responseBody response of
    Left err -> fatal . FailedToParseAuthChallenge $ toText err
    Right tokenResponse -> pure $ BearerAuthToken $ unToken tokenResponse
  where
    -- \| Authorization Server Endpoint.
    authTokenEndpoint :: Has (Lift IO) sig m => m (Request)
    authTokenEndpoint =
      sendIO $ parseRequest $ toString url <> "?" <> "service=" <> toString service <> "&scope=" <> toString scope
getTokenFromAuthChallenge cred (BasicAuthChallenge _) _ = do
  (user, pass) <- fromMaybeText "Registry requested basic auth challenge, but no credentials were provided!" cred
  pure $ BasicAuthToken user pass

data RegistryAuthChallenge
  = BasicAuthChallenge Text
  | BearerAuthChallenge RegistryBearerChallenge
  deriving (Show, Eq, Ord)

data RegistryBearerChallenge = RegistryBearerChallenge
  { authChallengeBearerRealm :: Text
  , authChallengeService :: Text
  , authChallengeScope :: Text
  }
  deriving (Show, Eq, Ord)

newtype AuthChallengeResponse = AuthChallengeResponse {unToken :: Text} deriving (Eq, Show, Ord)
instance FromJSON AuthChallengeResponse where
  parseJSON = withObject "AuthChallengeResponse" $ \o ->
    ( AuthChallengeResponse <$> o .: "token"
        <|> AuthChallengeResponse <$> o .: "access_token"
    )

-- | Parses Authorization Header.
--
-- >> parseTest parseAuthChallenge "Bearer realm=\"a\",service=\"b\",scope=\"c:pull\" = (a, b, c:pull)
-- >> parseTest parseAuthChallenge "Bearer realm=\"a\",service=\"b\",scope=\"c\" = (a, b, c)
--
-- Refer to: https://tools.ietf.org/html/rfc6750#section-3
-- -
parseAuthChallenge :: Parser RegistryAuthChallenge
parseAuthChallenge = do
  wwwProps <- propertiesParser

  let basicUrl = Map.lookup "Basic realm" wwwProps
  let bearerUrl = Map.lookup "Bearer realm" wwwProps
  let scope = Map.lookup "scope" wwwProps
  let service = Map.lookup "service" wwwProps

  case (basicUrl, bearerUrl, scope, service) of
    (Nothing, Just url', Just scope', Just service') -> pure $ BearerAuthChallenge $ RegistryBearerChallenge url' service' scope'
    (Just url', _, _, _) -> pure $ BasicAuthChallenge url'
    _ -> fail $ buildFailure basicUrl bearerUrl scope service
  where
    buildFailure :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> String
    buildFailure basicUrl bearerUrl scope service =
      "failed build registry challenge from 'Bearer realm' or 'Basic realm', 'scope', 'service' attributes."
        <> " Basic realm: "
        <> show basicUrl
        <> " Bearer realm: "
        <> show bearerUrl
        <> "; scope: "
        <> show scope
        <> "; service: "
        <> show service

    propertiesParser :: Parser (Map Text Text)
    propertiesParser = Map.fromList <$> sepBy propertyParser (char ',')

    propertyParser :: Parser (Text, Text)
    propertyParser = (,) <$> (keyParser <* chunk "=") <*> valueParser

    keyParser :: Parser Text
    keyParser = toText <$> some (alphaNumChar <|> char '_' <|> char '-' <|> char ' ')

    valueParser :: Parser Text
    valueParser = between "\"" "\"" (takeWhileP (Just "entry value") (not . (`elem` ("\"" :: String))))
