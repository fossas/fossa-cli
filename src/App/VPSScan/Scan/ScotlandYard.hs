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
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (viaShow)
import Network.HTTP.Req
import Text.URI (URI)
import qualified Text.URI as URI
import Prelude

newtype HTTP m a = HTTP {unHTTP :: m a}
  deriving (Functor, Applicative, Monad, MonadIO, Algebra sig)

data HTTPRequestFailed = HTTPRequestFailed HttpException
  deriving (Show)

instance ToDiagnostic HTTPRequestFailed where
  renderDiagnostic (HTTPRequestFailed exc) = "An HTTP request failed: " <> viaShow exc

runHTTP :: HTTP m a -> m a
runHTTP = unHTTP

-- parse a URI for use as a (base) Url, along with some default Options (e.g., port)
parseUri :: Has Diagnostics sig m => URI -> m (Url 'Https, Option 'Https)
parseUri uri = case useURI uri of
  Nothing -> fatalText ("Invalid URL: " <> URI.render uri)
  Just (Left (url, options)) -> pure (coerce url, coerce options)
  Just (Right (url, options)) -> pure (url, options)

instance (MonadIO m, Has Diagnostics sig m) => MonadHttp (HTTP m) where
  handleHttpException = HTTP . fatal . HTTPRequestFailed

-- /projects/{projectID}/scans
createScanEndpoint :: Url 'Https -> Text -> Url 'Https
createScanEndpoint baseurl projectId = baseurl /: "projects" /: projectId /: "scans"

-- /projects/{projectID}/scans/{scanID}/discovered_licenses
scanDataEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
scanDataEndpoint baseurl projectId scanId = baseurl /: "projects" /: projectId /: "scans" /: scanId /: "discovered_licenses"

data ScanResponse = ScanResponse
  { responseScanId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse <$> obj .: "scanId"

createScotlandYardScan :: (MonadIO m, Has Diagnostics sig m) => VPSOpts -> m ScanResponse
createScotlandYardScan VPSOpts {..} = runHTTP $ do
  let body = object ["organizationId" .= organizationID, "revisionId" .= revisionID, "projectId" .= projectID]
      ScotlandYardOpts {..} = vpsScotlandYard

  (baseUrl, baseOptions) <- parseUri scotlandYardUrl
  resp <- req POST (createScanEndpoint baseUrl projectID) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json")
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IPR result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
uploadIPRResults :: (ToJSON a, MonadIO m, Has Diagnostics sig m) => VPSOpts -> Text -> a -> m ()
uploadIPRResults VPSOpts {..} scanId value = runHTTP $ do
  let ScotlandYardOpts {..} = vpsScotlandYard

  (baseUrl, baseOptions) <- parseUri scotlandYardUrl

  _ <- req POST (scanDataEndpoint baseUrl projectID scanId) (ReqBodyJson value) ignoreResponse (baseOptions <> header "Content-Type" "application/json")
  pure ()
