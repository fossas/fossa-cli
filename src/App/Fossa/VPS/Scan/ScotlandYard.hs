{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.ScotlandYard
  ( uploadBuildGraph
  , getScan
  , getLatestScan
  , ScanResponse (..)
  , ScotlandYardNinjaOpts (..)
  )
where

import App.Fossa.VPS.Scan.Core
import App.Fossa.VPS.Types
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Carrier.StickyLogger (runStickyLogger, StickyLogger, logSticky')
import Control.Carrier.TaskPool
import Control.Effect.Lift
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (traverse_)
import Data.Text (Text)
import Effect.Logger
import Fossa.API.Types (ApiOpts, useApiOpts)
import GHC.Conc.Sync (getNumCapabilities)
import Network.HTTP.Req

data ScotlandYardNinjaOpts = ScotlandYardNinjaOpts
  { syNinjaProjectId :: Locator
  , syNinjaOrganizationId :: Int
  , syNinjaOpts :: NinjaGraphOpts
  }

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

getScanEndpoint :: Url 'Https -> Locator -> Text -> Url 'Https
getScanEndpoint baseurl (Locator projectId) scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId

getLatestScanEndpoint :: Url 'Https -> Locator -> Url 'Https
getLatestScanEndpoint baseurl (Locator projectId) = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: "latest"

newtype CreateScanResponse = CreateScanResponse
  { createScanResponseId :: Text
  } deriving (Eq, Ord, Show)

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
  let opts = baseOptions
        <> header "Content-Type" "application/json"
        <> "revisionID" =: revisionId
  resp <- req GET (getLatestScanEndpoint baseUrl locator) NoReqBody jsonResponse opts
  pure (responseBody resp)

getScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Locator -> Text -> m ScanResponse
getScan apiOpts locator scanId = runHTTP $ do
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let opts = baseOptions
        <> header "Content-Type" "application/json"
  resp <- req GET (getScanEndpoint baseUrl locator scanId) NoReqBody jsonResponse opts
  pure (responseBody resp)

-- /projects/{projectID}/scans/{scanID}/build-graphs
createBuildGraphEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
createBuildGraphEndpoint baseurl projectId scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "build-graphs"

-- /projects/{projectID}/scans/{scanID}/build-graphs/{buildGraphID}/rules
uploadBuildGraphChunkEndpoint :: Url 'Https -> Text -> Text -> Text ->  Url 'Https
uploadBuildGraphChunkEndpoint baseurl projectId scanId buildGraphId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "build-graphs" /: buildGraphId /: "rules"

-- /projects/{projectID}/scans/{scanID}/build-graphs/{buildGraphID}/rules/complete
uploadBuildGraphCompleteEndpoint :: Url 'Https -> Text -> Text -> Text ->  Url 'Https
uploadBuildGraphCompleteEndpoint baseurl projectId scanId buildGraphId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "build-graphs" /: buildGraphId /: "rules" /: "complete"

-- create the build graph in SY, upload it in chunks of ~ 1 MB and then complete it.
uploadBuildGraph :: (Has (Lift IO) sig m, Has Logger sig m, Has Diagnostics sig m) => ApiOpts -> ScotlandYardNinjaOpts -> [DepsTarget] -> m ()
uploadBuildGraph apiOpts syOpts@ScotlandYardNinjaOpts {..} targets = runHTTP $ do
  let NinjaGraphOpts{..} = syNinjaOpts
      locator = unLocator syNinjaProjectId
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let authenticatedHttpOptions = baseOptions <> header "Content-Type" "application/json"

  -- create the build graph and save its ID
  let createUrl = createBuildGraphEndpoint baseUrl locator scanId
  buildGraphId <- createBuildGraph syOpts createUrl authenticatedHttpOptions

  -- split the build graph data into chunks and upload it
  let chunkUrl = uploadBuildGraphChunkEndpoint baseUrl locator scanId (responseBuildGraphId buildGraphId)
      chunkedTargets = chunkedBySize targets (1024 * 1024)
  capabilities <- liftIO getNumCapabilities
  _ <- runStickyLogger . withTaskPool capabilities updateProgress $
    traverse_ (forkTask . uploadBuildGraphChunk chunkUrl authenticatedHttpOptions) chunkedTargets

  -- mark the build graph as complete
  _ <- req PUT (uploadBuildGraphCompleteEndpoint baseUrl locator scanId (responseBuildGraphId buildGraphId)) (ReqBodyJson $ object []) ignoreResponse authenticatedHttpOptions
  pure ()

createBuildGraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ScotlandYardNinjaOpts -> Url 'Https -> Option 'Https -> m CreateBuildGraphResponse
createBuildGraph ScotlandYardNinjaOpts {..} url httpOptions = runHTTP $ do
  let NinjaGraphOpts{..} = syNinjaOpts
  let body = object ["display_name" .= buildName]
  resp <- req POST url (ReqBodyJson body) jsonResponse httpOptions
  pure (responseBody resp)

uploadBuildGraphChunk :: (Has (Lift IO) sig m) => Url 'Https -> Option 'Https -> [DepsTarget] -> m ()
uploadBuildGraphChunk url httpOptions targets = do
  let jsonChunk = object ["targets" .= targets]
  _ <- sendIO $ runDiagnostics $ runHTTP $ req POST url (ReqBodyJson jsonChunk) ignoreResponse httpOptions
  pure ()

updateProgress :: Has StickyLogger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky'
    ( "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )

-- chunk a list of Values by their size, trying to keep each chunk of values
-- under maxByteSize. This is not guaranteed if one of the elements in the list is
-- greater than maxByteSize.
chunkedBySize :: (ToJSON a) => [a] -> Int -> [[a]]
chunkedBySize d maxByteSize =
    chunked
  where
    (_, chunked) = foldr (addToList maxByteSize) (0, [[]]) d
    addToList :: (ToJSON a) => Int -> a -> (Int, [[a]]) -> (Int, [[a]])
    addToList maxLength ele (currentLength, first:rest) =
      if (currentLength + newLength) > maxLength then
        (newLength, [ele]:first:rest)
      else
        (newLength + currentLength, (ele:first):rest)
      where
        newLength = fromIntegral $ BS.length $ encode ele
    addToList _ ele (n, []) = (n, [[ele]])
