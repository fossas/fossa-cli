{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.Telemetry.Sink.Endpoint (sinkTelemetryToEndpoint, sinkToEndpoint, sinkToLocalhost) where

import App.Fossa.Telemetry.Types (TelemetryRecord)
import Control.Algebra (Algebra, Has, type (:+:))
import Control.Carrier.Empty.Maybe (EmptyC, runEmpty)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Effect.Empty (Empty, empty)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (encodeUtf8)
import Fossa.API.Types (
  ApiKey (unApiKey),
  ApiOpts (apiOptsApiKey, apiOptsUri),
 )
import Network.HTTP.Req (
  MonadHttp (..),
  Option,
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  Scheme (Https),
  Url,
  header,
  http,
  ignoreResponse,
  port,
  req,
  useURI,
  (/:),
 )
import Network.HTTP.Req.Extra (httpConfigRetryTimeouts)
import Text.URI (URI)
import Text.URI.QQ (uri)
import Unsafe.Coerce qualified as Unsafe

newtype FossaTelemetryReq m a = FossaTelemetryReq {unFossaTelemetryReq :: EmptyC m a}
  deriving (Functor, Applicative, Monad, Algebra (Empty :+: sig))

instance Has (Lift IO) sig m => MonadIO (FossaTelemetryReq m) where
  liftIO = sendIO

instance (Has (Lift IO) sig m) => MonadHttp (FossaTelemetryReq m) where
  getHttpConfig = pure httpConfigRetryTimeouts

  -- We fire and forget responses (including http exception or connection re issues)
  -- We do this as _telemetry_ failure should not have any visible or material impact on user experience.
  handleHttpException = FossaTelemetryReq . const empty

useApiOpts :: ApiOpts -> Maybe (Url 'Https, Option 'Https)
useApiOpts opts = case useURI serverURI of
  Nothing -> Nothing -- Do not emit telemetry if user provided endpoint is not valid
  Just (Left (url, options)) -> Just (Unsafe.unsafeCoerce url, coerce options <> authHeader (apiOptsApiKey opts))
  Just (Right (url, options)) -> Just (url, options <> authHeader (apiOptsApiKey opts))
  where
    serverURI :: URI
    serverURI = fromMaybe [uri|https://app.fossa.com|] (apiOptsUri opts)

    authHeader :: ApiKey -> Option 'Https
    authHeader key = header "Authorization" (encodeUtf8 ("Bearer " <> unApiKey key))

sinkTelemetryToEndpoint :: Has (Lift IO) sig m => ApiOpts -> TelemetryRecord -> m ()
sinkTelemetryToEndpoint = sinkToNothing

telemetryUrl :: Url scheme -> Url scheme
telemetryUrl baseurl = baseurl /: "api" /: "cli" /: "telemetry"

sinkToEndpoint :: Has (Lift IO) sig m => ApiOpts -> TelemetryRecord -> m ()
sinkToEndpoint apiOpts record =
  void
    . runEmpty
    . unFossaTelemetryReq
    $ case useApiOpts apiOpts of
      Nothing -> pure ()
      Just (baseUrl, opts) ->
        void $
          req
            POST
            (telemetryUrl baseUrl)
            (ReqBodyJson record)
            ignoreResponse
            opts

sinkToNothing :: Has (Lift IO) sig m => ApiOpts -> TelemetryRecord -> m ()
sinkToNothing _ _ = pure ()

sinkToLocalhost :: Has (Lift IO) sig m => ApiOpts -> TelemetryRecord -> m ()
sinkToLocalhost _ record =
  void
    . runEmpty
    . unFossaTelemetryReq
    . void
    $ req
      POST
      (telemetryUrl $ http "localhost")
      (ReqBodyJson record)
      ignoreResponse
      (port 8866)
