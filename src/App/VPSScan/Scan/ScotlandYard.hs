module App.VPSScan.Scan.ScotlandYard
  ( HTTP (..),
    runHTTP,
    ScanResponse (..),
    createScotlandYardScan,
    uploadIPRResults,
  )
where

import App.VPSScan.Types
import Control.Effect.Diagnostics
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Req
import Prelude
import App.Util (parseUri)
import Data.Text.Encoding

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

authHeader :: Text -> Option scheme
authHeader apiKey = header "Authorization" (encodeUtf8 ("Bearer " <> apiKey))

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

createScotlandYardScan :: (MonadIO m, Has Diagnostics sig m) => VPSOpts -> m ScanResponse
createScotlandYardScan VPSOpts {..} = runHTTP $ do
  let body = object ["organizationId" .= organizationID, "revisionId" .= revisionID, "projectId" .= projectID]; FossaOpts {..} = fossaInstance
  let auth = authHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req POST (createScanEndpoint baseUrl projectID) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IPR result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
uploadIPRResults :: (ToJSON a, MonadIO m, Has Diagnostics sig m) => VPSOpts -> Text -> a -> m ()
uploadIPRResults VPSOpts {..} scanId value = runHTTP $ do
  let FossaOpts {..} = fossaInstance
  let auth = authHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl

  _ <- req POST (scanDataEndpoint baseUrl projectID scanId) (ReqBodyJson value) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()
