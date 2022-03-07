{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.Telemetry.Sink.Endpoint (sinkTelemetryToEndpoint) where

import App.Fossa.Telemetry.Types (TelemetryRecord)
import Control.Algebra
import Control.Carrier.Empty.Maybe (EmptyC, runEmpty)
import Control.Carrier.Lift
import Control.Effect.Empty
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Coerce
import Data.Maybe (fromMaybe)
import Data.String.Conversion (encodeUtf8)
import Fossa.API.Types hiding (useApiOpts)
import Network.HTTP.Req
import Network.HTTP.Req.Extra
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
