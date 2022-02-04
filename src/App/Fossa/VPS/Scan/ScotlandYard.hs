{-# LANGUAGE DataKinds #-}

module App.Fossa.VPS.Scan.ScotlandYard (
  getScan,
  getLatestScan,
  ScanResponse (..),
) where

import App.Fossa.VPS.Scan.Core (Locator (Locator))
import App.Fossa.VPS.Types (runHTTP)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Stack (Has)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Text (Text)
import Fossa.API.Types (ApiOpts, useApiOpts)
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

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

getScanEndpoint :: Url 'Https -> Locator -> Text -> Url 'Https
getScanEndpoint baseurl (Locator projectId) scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId

getLatestScanEndpoint :: Url 'Https -> Locator -> Url 'Https
getLatestScanEndpoint baseurl (Locator projectId) = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: "latest"

newtype CreateScanResponse = CreateScanResponse
  { createScanResponseId :: Text
  }
  deriving (Eq, Ord, Show)

data ScanResponse = ScanResponse
  { responseScanId :: Text
  , responseScanStatus :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CreateScanResponse where
  parseJSON = withObject "CreateScanResponse" $ \obj ->
    CreateScanResponse
      <$> obj .: "scanId"

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse
      <$> obj .: "id"
      <*> obj .:? "status"

newtype CreateBuildGraphResponse = CreateBuildGraphResponse
  { responseBuildGraphId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CreateBuildGraphResponse where
  parseJSON = withObject "CreateBuildGraphResponse" $ \obj ->
    CreateBuildGraphResponse <$> obj .: "id"

getLatestScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Locator -> Text -> m ScanResponse
getLatestScan apiOpts locator revisionId = runHTTP $ do
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let opts =
        baseOptions
          <> header "Content-Type" "application/json"
          <> "revisionID" =: revisionId
  resp <- req GET (getLatestScanEndpoint baseUrl locator) NoReqBody jsonResponse opts
  pure (responseBody resp)

getScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Locator -> Text -> m ScanResponse
getScan apiOpts locator scanId = runHTTP $ do
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let opts =
        baseOptions
          <> header "Content-Type" "application/json"
  resp <- req GET (getScanEndpoint baseUrl locator scanId) NoReqBody jsonResponse opts
  pure (responseBody resp)
