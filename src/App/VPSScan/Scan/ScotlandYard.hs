module App.VPSScan.Scan.ScotlandYard
  ( createScan
  , postIprResults
  , ScotlandYardOpts(..)
  , HTTP(..)
  , runHTTP
  , ScanResponse(..)
  , ScotlandYard(..)
  , ScotlandYardC(..)
  , createScotlandYardScan
  , uploadIPRResults
  )
where
import Prologue

import Control.Algebra
import Control.Carrier.Error.Either
import Network.HTTP.Req

data ScotlandYardOpts = ScotlandYardOpts
  { scotlandYardUrl :: Url 'Https
  , scotlandYardPort :: Int
  , organizationID :: Text
  , projectID :: Text
  , revisionID :: Text
  } deriving (Eq, Ord, Show, Generic)

newtype HTTP a = HTTP { unHTTP :: ErrorC HttpException IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runHTTP :: MonadIO m => HTTP a -> m (Either HttpException a)
runHTTP = liftIO . runError @HttpException . unHTTP

instance MonadHttp HTTP where
  handleHttpException = HTTP . throwError

-- /projects/{projectID}/scans
createScanEndpoint :: Url 'Https -> Text -> Url 'Https
createScanEndpoint baseurl projectId = baseurl /: "projects" /: projectId /: "scans"

-- /projects/{projectID}/scans/{scanID}/discovered_licenses
scanDataEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
scanDataEndpoint baseurl projectId scanId = baseurl /: "projects" /: projectId /: "scans" /: scanId /: "discovered_licenses"

data ScanResponse = ScanResponse
  { responseScanId :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse <$> obj .: "scanId"

createScan :: ScotlandYardOpts -> HTTP ScanResponse
createScan ScotlandYardOpts{..} = do
  let body = object ["organizationId" .= organizationID, "revisionId" .= revisionID]
  resp <- req POST (createScanEndpoint scotlandYardUrl projectID) (ReqBodyJson body) jsonResponse $ port scotlandYardPort
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IRP result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
postIprResults :: ToJSON a => ScotlandYardOpts -> Text -> a -> HTTP ()
postIprResults ScotlandYardOpts{..} scanId value = do
  _ <- req POST (scanDataEndpoint scotlandYardUrl projectID scanId) (ReqBodyJson value) ignoreResponse $ port scotlandYardPort
  pure ()

----- scotland yard effect

data ScotlandYard m k
  = CreateScotlandYardScan ScotlandYardOpts (Either HttpException ScanResponse -> m k) -- TODO: add Scotland yard error type
  | UploadIPRResults ScotlandYardOpts Text Array (Either HttpException () -> m k) -- TODO: add scotland yard error type
  deriving Generic1

instance HFunctor ScotlandYard
instance Effect ScotlandYard

createScotlandYardScan :: Has ScotlandYard sig m => ScotlandYardOpts -> m (Either HttpException ScanResponse)
createScotlandYardScan opts = send (CreateScotlandYardScan opts pure)

uploadIPRResults :: Has ScotlandYard sig m => ScotlandYardOpts -> Text -> Array -> m (Either HttpException ())
uploadIPRResults opts scanId value = send (UploadIPRResults opts scanId value pure)

----- scotland yard production interpreter

newtype ScotlandYardC m a = ScotlandYardC { runScotlandYard :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (ScotlandYard :+: sig) (ScotlandYardC m) where
  alg (R other) = ScotlandYardC (alg (handleCoercible other))
  alg (L (CreateScotlandYardScan scotlandYardOpts k)) = (k =<<) . ScotlandYardC $ runHTTP $ createScan scotlandYardOpts
  alg (L (UploadIPRResults scotlandYardOpts scanId value k)) = (k =<<) . ScotlandYardC $ runHTTP $ postIprResults scotlandYardOpts scanId value
