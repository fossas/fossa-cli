{-# LANGUAGE DataKinds #-}

module Control.Carrier.FossaApiClient.Internal.ScotlandYard (
  getScan,
  getLatestScan,
) where

import App.Fossa.VPS.Types (runHTTP)
import App.Types (ProjectRevision (..))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Control.Effect.Stack (Has)
import Fossa.API.Types (ApiOpts, ScanId (ScanId), ScanResponse, useApiOpts, Organization (organizationId), OrgId)
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
import Srclib.Types (Locator)
import App.Fossa.FossaAPIV1 (renderLocatorUrl, getOrganization)

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

getScanEndpoint :: Url 'Https -> OrgId -> Locator -> ScanId -> Url 'Https
getScanEndpoint baseurl orgId locator (ScanId scanId) = coreProxyPrefix baseurl /: "projects" /: renderLocatorUrl orgId locator /: "scans" /: scanId

getLatestScanEndpoint :: Url 'Https -> OrgId -> Locator -> Url 'Https
getLatestScanEndpoint baseurl orgId locator = coreProxyPrefix baseurl /: "projects" /: renderLocatorUrl orgId locator /: "scans" /: "latest"

getLatestScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Locator ->
  ProjectRevision ->
  m ScanResponse
getLatestScan locator ProjectRevision{projectRevision} = runHTTP $ do
  apiOpts <- ask
  orgId <- organizationId <$> getOrganization apiOpts
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let opts =
        baseOptions
          <> header "Content-Type" "application/json"
          <> "revisionID" =: projectRevision
  resp <- req GET (getLatestScanEndpoint baseUrl orgId locator) NoReqBody jsonResponse opts
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
  orgId <- organizationId <$> getOrganization apiOpts
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let opts =
        baseOptions
          <> header "Content-Type" "application/json"
  resp <- req GET (getScanEndpoint baseUrl orgId locator scanId) NoReqBody jsonResponse opts
  pure (responseBody resp)
