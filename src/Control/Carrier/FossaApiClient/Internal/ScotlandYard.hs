{-# LANGUAGE DataKinds #-}

module Control.Carrier.FossaApiClient.Internal.ScotlandYard (
  getScan,
  getLatestScan,
) where

import Control.Effect.Reader (Reader, ask)
import App.Fossa.VPS.Types (runHTTP)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Stack (Has)
import Fossa.API.Types (ScanResponse, useApiOpts, ScanId (ScanId), ApiOpts)
import Network.HTTP.Req (
  GET (GET),
  NoReqBody (NoReqBody),
  Scheme (Https),
  Url,
  header,
  jsonResponse,
  req,
  responseBody,
  (/:),
  (=:),
 )
import Srclib.Types (Locator, renderLocator)
import App.Types (ProjectRevision(..))

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

getScanEndpoint :: Url 'Https -> Locator -> ScanId -> Url 'Https
getScanEndpoint baseurl locator (ScanId scanId) = coreProxyPrefix baseurl /: "projects" /: renderLocator locator /: "scans" /: scanId

getLatestScanEndpoint :: Url 'Https -> Locator -> Url 'Https
getLatestScanEndpoint baseurl locator = coreProxyPrefix baseurl /: "projects" /: renderLocator locator /: "scans" /: "latest"

getLatestScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Locator ->
  ProjectRevision ->
  m ScanResponse
getLatestScan locator ProjectRevision{projectRevision}  = runHTTP $ do
  apiOpts <- ask
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let opts =
        baseOptions
          <> header "Content-Type" "application/json"
          <> "revisionID" =: projectRevision
  resp <- req GET (getLatestScanEndpoint baseUrl locator) NoReqBody jsonResponse opts
  pure (responseBody resp)

getScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Locator ->
  ScanId ->
  m ScanResponse
getScan locator scanId = runHTTP $ do
  apiOpts <- ask
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let opts =
        baseOptions
          <> header "Content-Type" "application/json"
  resp <- req GET (getScanEndpoint baseUrl locator scanId) NoReqBody jsonResponse opts
  pure (responseBody resp)
