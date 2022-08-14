module Control.Carrier.ContainerRegistryApi.Authorization (
  getAuthToken,
  getAuthToken',

  -- * for testing
  RegistryAuthChallenge (..),
  parseAuthChallenge,
) where

import Control.Algebra (Has)
import Control.Carrier.ContainerRegistryApi.Common (
  fromResponse,
  getHeaderValue,
  http,
  logHttp,
  originalReqUri,
 )
import Control.Carrier.ContainerRegistryApi.Errors (
  ContainerRegistryApiErrorBody,
  FailedToParseAuthChallenge (FailedToParseAuthChallenge),
  UnknownApiError (UnknownApiError),
 )
import Control.Effect.Diagnostics (Diagnostics, fatal, fatalText)
import Control.Effect.Lift (Lift, sendIO)
import Data.Aeson (FromJSON (parseJSON), decode', eitherDecode, withObject, (.:))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (encodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Void (Void)
import Effect.Logger (Logger)
import Network.HTTP.Client (
  Manager,
  Request (method),
  Response (responseBody, responseHeaders, responseStatus),
  applyBasicAuth,
  parseRequest,
 )
import Network.HTTP.Types (statusCode)
import Network.HTTP.Types.Header (hWWWAuthenticate)
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

data RegistryAuthChallenge = RegistryAuthChallenge
  { authChallengeBearerRealm :: Text
  , authChallengeService :: Text
  , authChallengeScope :: Text
  }
  deriving (Show, Eq, Ord)

newtype AuthChallengeResponse = AuthChallengeResponse {token :: Text} deriving (Eq, Show, Ord)
instance FromJSON AuthChallengeResponse where
  parseJSON = withObject "" $ \o -> AuthChallengeResponse <$> o .: "token"

-- | Authorization Server Endpoint.
-- Refer to:
authTokenEndpoint :: Has (Lift IO) sig m => RegistryAuthChallenge -> m (Request)
authTokenEndpoint (RegistryAuthChallenge url service scope) =
  sendIO $ parseRequest $ toString url <> "?" <> "service=" <> toString service <> "&scope=" <> toString scope

getAuthToken ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Maybe (Text, Text) ->
  -- | Username and Password to user when retrieving authorization token
  Request ->
  -- | Request For which to Get Authorization Token
  Manager ->
  -- | Manager to use for requests
  m (Maybe Text)
getAuthToken = getAuthToken' True

-- | Generates Auth Token For Request.
--
-- Refer to:
--
--  Docker Registry Spec: https://docs.docker.com/registry/spec/auth/token/
--  OCI Spec:
--
-- OCI Registry SPEC do not explicitly specify auth workflow, but all registries tested follow
-- specified workflow, same as docker registry.
--
-- In Summary, Auth Workflow is:
--
--  1. Client attempts request to desired endpoint (recommendation is to mae HEAD request)
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
getAuthToken' ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Bool ->
  Maybe (Text, Text) ->
  Request ->
  Manager ->
  m (Maybe Text)
getAuthToken' includeDebug cred reqAttempt manager = do
  -- We Attempt to make HEAD request to see if the
  -- endpoint is accessible. If we need to pass auth challenge,
  -- we will see unauthorized failure. `HEAD` is supported for
  -- all endpoints per SPEC.
  let request' = reqAttempt{method = "HEAD"}
  headResponse <- (if includeDebug then logHttp else http) request' manager

  let respBody = responseBody headResponse
  let errs :: Maybe ContainerRegistryApiErrorBody = decode' respBody

  case (errs, statusCode . responseStatus $ headResponse) of
    -- If Registry does not have auth challenge, we will see successful response.
    -- meaning that we do not need token for provided request.
    (Nothing, 200) -> pure Nothing
    (_, 401) -> do
      case parse parseAuthChallenge "" <$> getHeaderValue hWWWAuthenticate (responseHeaders headResponse) of
        Nothing ->
          fatalText
            ( "Did not provide WWW-Authenticate Challenge to Retrieve Authorization Token: "
                <> (toText . show $ originalReqUri headResponse)
            )
        Just (Left err) -> fatalText . toText $ errorBundlePretty err
        Just (Right authChallenge) -> do
          authToken <- getTokenFromAuthChallenge includeDebug cred authChallenge manager
          pure $ Just authToken
    (Just apiErrors, _) -> fatal (originalReqUri headResponse, apiErrors)
    (Nothing, _) -> fatal $ UnknownApiError (originalReqUri headResponse) $ responseStatus headResponse

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
  Bool ->
  -- | Should Debug Request?
  Maybe (Text, Text) ->
  -- | Username and Password to user when retrieving authorization token
  RegistryAuthChallenge ->
  -- | Authorization Challenge
  Manager ->
  -- | Request Manager to use for subsequent requests
  m Text
getTokenFromAuthChallenge includeDebug cred (RegistryAuthChallenge url service scope) manager = do
  req <- authTokenEndpoint (RegistryAuthChallenge url service scope)

  let req' = case cred of
        Nothing -> req
        Just (user, pass) -> applyBasicAuth (encodeUtf8 user) (encodeUtf8 pass) req

  response <- fromResponse =<< (if includeDebug then logHttp else http) req' manager
  case eitherDecode (responseBody response) of
    Left err -> fatal . FailedToParseAuthChallenge $ toText err
    Right (AuthChallengeResponse token) -> pure token

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

  let url = Map.lookup "Bearer realm" wwwProps
  let scope = Map.lookup "scope" wwwProps
  let service = Map.lookup "service" wwwProps

  case (url, scope, service) of
    (Just url', Just scope', Just service') -> pure $ RegistryAuthChallenge url' service' scope'
    _ -> fail $ buildFailure url scope service
  where
    buildFailure :: Maybe Text -> Maybe Text -> Maybe Text -> String
    buildFailure url scope service =
      "failed to parse all of required 'Bearer realm', 'scope', 'service' attributes."
        <> " Bearer realm: "
        <> show url
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
