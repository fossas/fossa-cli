{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.ScotlandYard
  ( createScotlandYardScan
  , uploadIPRResults
  , uploadBuildGraph
  , ScanResponse (..)
  , ScotlandYardOpts (..)
  , ScotlandYardNinjaOpts (..)
  )
where

import Control.Carrier.TaskPool
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Monad.IO.Class
import Control.Effect.Lift
import App.Fossa.VPS.Types
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import Effect.Logger
import GHC.Conc.Sync (getNumCapabilities)
import App.Fossa.VPS.Scan.Core
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

data ScotlandYardNinjaOpts = ScotlandYardNinjaOpts
  { syNinjaProjectId :: Locator
  , syNinjaOrganizationId :: Int
  , syNinjaOpts :: NinjaGraphOpts
  }

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

-- /projects/{projectID}/scans
createScanEndpoint :: Url 'Https -> Text -> Url 'Https
createScanEndpoint baseurl projectId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans"

-- /projects/{projectID}/scans/{scanID}/discovered_licenses/partial
uploadIPRChunkEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
uploadIPRChunkEndpoint baseurl projectId scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "discovered_licenses" /: "partial"

-- /projects/{projectID}/scans/{scanID}/discovered_licenses/complete
uploadIPRCompleteEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
uploadIPRCompleteEndpoint baseurl projectId scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "discovered_licenses" /: "complete"

data ScanResponse = ScanResponse
  { responseScanId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse <$> obj .: "scanId"

data CreateBuildGraphResponse = CreateBuildGraphResponse
  { responseBuildGraphId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CreateBuildGraphResponse where
  parseJSON = withObject "CreateBuildGraphResponse" $ \obj ->
    CreateBuildGraphResponse <$> obj .: "id"

createScotlandYardScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ScotlandYardOpts -> m ScanResponse
createScotlandYardScan ScotlandYardOpts {..} = runHTTP $ do
  let VPSOpts{..} = syVpsOpts
      FossaOpts{..} = fossa
      body = object ["revisionId" .= projectRevision, "organizationId" .= organizationId]
      auth = coreAuthHeader fossaApiKey
      locator = unLocator projectId

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req POST (createScanEndpoint baseUrl locator) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IPR result in chunks of ~ 1 MB to the "Upload IPR Data" endpoint on Scotland Yard.
-- once all of the chunks are complete, PUT to the "upload IPR data complete" endpoint,
uploadIPRResults :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> Value -> ScotlandYardOpts -> m ()
uploadIPRResults scanId value ScotlandYardOpts {..} = runHTTP $ do
  let VPSOpts{..} = syVpsOpts
      FossaOpts{..} = fossa
      auth = coreAuthHeader fossaApiKey
      locator = unLocator projectId
  (baseUrl, baseOptions) <- parseUri fossaUrl
  let url = uploadIPRChunkEndpoint baseUrl locator scanId
      authenticatedHttpOptions = baseOptions <> header "Content-Type" "application/json" <> auth
      chunkedJSON = fromMaybe [] (chunkJSON value "Files" (1024 * 1024))

  capabilities <- liftIO getNumCapabilities
  _ <- liftIO $ withLogger SevTrace $ withTaskPool capabilities updateProgress $ traverse_ (forkTask . uploadIPRChunk url authenticatedHttpOptions) chunkedJSON
  _ <- req PUT (uploadIPRCompleteEndpoint baseUrl locator scanId) (ReqBodyJson $ object []) ignoreResponse authenticatedHttpOptions
  pure ()

uploadIPRChunk :: (Has (Lift IO) sig m) => Url 'Https -> Option 'Https -> Value -> m ()
uploadIPRChunk url httpOptions jsonChunk = do
  _ <- sendIO $ runDiagnostics $ runHTTP $ req POST url (ReqBodyJson jsonChunk) ignoreResponse httpOptions
  pure ()

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
uploadBuildGraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ScotlandYardNinjaOpts -> [DepsTarget] -> m ()
uploadBuildGraph syOpts@ScotlandYardNinjaOpts {..} targets = runHTTP $ do
  let NinjaGraphOpts{..} = syNinjaOpts
      FossaOpts{..} = ninjaFossaOpts
      auth = coreAuthHeader fossaApiKey
      locator = unLocator syNinjaProjectId
  (baseUrl, baseOptions) <- parseUri fossaUrl
  let authenticatedHttpOptions = baseOptions <> header "Content-Type" "application/json" <> auth

  -- create the build graph and save its ID
  let createUrl = createBuildGraphEndpoint baseUrl locator scanId
  buildGraphId <- createBuildGraph syOpts createUrl authenticatedHttpOptions

  -- split the build graph data into chunks and upload it
  let chunkUrl = uploadBuildGraphChunkEndpoint baseUrl locator scanId (responseBuildGraphId buildGraphId)
      chunkedTargets = chunkedBySize targets (1024 * 1024)
  capabilities <- liftIO getNumCapabilities
  _ <- liftIO $ withLogger SevTrace $ withTaskPool capabilities updateProgress $ traverse_ (forkTask . uploadBuildGraphChunk chunkUrl authenticatedHttpOptions) chunkedTargets

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

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )

chunkJSON :: Value -> Text -> Int -> Maybe [Value]
chunkJSON (Object obj) key chunkSize = do
  a <- HM.lookup key obj
  arr <- case a of
    Array aa -> Just aa
    _ -> Nothing
  let chunker :: [Value] -> Value
      chunker v = object [key .= v]
      chunked = chunkedBySize (V.toList arr) chunkSize
  Just $ map chunker chunked

chunkJSON _ _ _ = Nothing

-- chunk a list of Values by their size, trying to keep each chunk of values
-- under maxByteSize. This is not guaranteed if one of the elements in the list is
-- greater than maxByteSize.
chunkedBySize :: (ToJSON a) => [a] -> Int -> [[a]]
chunkedBySize d maxByteSize =
    chunked
  where
    (_, chunked) = foldr (addToList maxByteSize) (0, [[]]) d
    addToList :: (ToJSON a) => Int -> a -> (Int, [[a]]) -> (Int, [[a]])
    addToList maxLength ele (currentLength, (first:rest)) =
      if (currentLength + newLength) > maxLength then
        (newLength, [ele]:first:rest)
      else
        (newLength + currentLength, (ele:first):rest)
      where
        newLength = fromIntegral $ BS.length $ encode ele
    addToList _ ele (n, []) = (n, [[ele]])
