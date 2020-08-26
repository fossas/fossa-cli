{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module App.VPSScan.Scan.ScotlandYard
  ( createScotlandYardScan
  , uploadIPRResults
  , ScanResponse (..)
  , ScotlandYardOpts (..)
  )
where

import App.VPSScan.Types
import App.VPSScan.Scan.Core
import Control.Effect.Diagnostics
import Control.Effect.Lift (Lift)
import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Req
import App.Util (parseUri)

data ScotlandYardOpts = ScotlandYardOpts
  { projectId :: Locator
  , projectRevision :: Text
  , organizationId :: Int
  , syVpsOpts :: VPSOpts
  }

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

-- /projects/{projectID}/scans
createScanEndpoint :: Url 'Https -> Text -> Url 'Https
createScanEndpoint baseurl projectId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans"

-- /projects/{projectID}/scans/{scanID}/discovered_licenses
scanDataEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
scanDataEndpoint baseurl projectId scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "discovered_licenses"

data ScanResponse = ScanResponse
  { responseScanId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse <$> obj .: "scanId"

createScotlandYardScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ScotlandYardOpts -> m ScanResponse
createScotlandYardScan ScotlandYardOpts {..} = runHTTP $ do
  let VPSOpts{..} = syVpsOpts
  let FossaOpts{..} = fossa
  
  let body = object ["revisionId" .= projectRevision, "organizationId" .= organizationId]
  let auth = coreAuthHeader fossaApiKey
  let locator = unLocator projectId

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req POST (createScanEndpoint baseUrl locator) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IPR result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
uploadIPRResults :: (ToJSON a, Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> a -> ScotlandYardOpts -> m ()
uploadIPRResults scanId value ScotlandYardOpts {..} = runHTTP $ do
  let VPSOpts{..} = syVpsOpts
  let FossaOpts{..} = fossa
  let auth = coreAuthHeader fossaApiKey
  let locator = unLocator projectId

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (scanDataEndpoint baseUrl locator scanId) (ReqBodyJson value) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()
