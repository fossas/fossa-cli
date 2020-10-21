{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.Core
  ( coreAuthHeader
  , createCoreProject
  , completeCoreProject
  , getSherlockInfo
  , createLocator
  , createRevisionLocator
  , buildRevision
  , overrideScanFilters
  , storeUpdatedScanFilters
  , Locator(..)
  , RevisionLocator(..)
  , SherlockInfo(..)
  )
where

import App.Fossa.VPS.Types
import App.Util (parseUri)
import Data.Text (pack, Text)
import Prelude
import Network.HTTP.Req
import Control.Carrier.Trace.Printing
import Control.Effect.Diagnostics
import Control.Effect.Lift (Lift, sendIO)
import Data.Aeson
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text.Encoding
import Effect.Logger

data SherlockInfo = SherlockInfo
  { sherlockUrl :: Text
  , sherlockClientToken :: Text
  , sherlockClientId :: Text
  , sherlockOrgId :: Int
  }
  deriving (Eq, Ord, Show)

instance FromJSON SherlockInfo where
  parseJSON = withObject "SherlockInfo" $ \obj -> do
    auth <- obj .: "auth"
    SherlockInfo <$> obj .: "url" <*> auth .: "clientToken" <*> auth .: "clientId" <*> obj .: "orgId"

coreAuthHeader :: Text -> Option scheme
coreAuthHeader apiKey = header "Authorization" (encodeUtf8 ("Bearer " <> apiKey))

buildRevision :: Has (Lift IO) sig m => Maybe Text -> m Text
buildRevision (Just userProvidedRevision) = pure userProvidedRevision
buildRevision Nothing = do
  posixTime <- sendIO getPOSIXTime
  pure (pack $ show (floor $ toRational posixTime :: Int))

newtype Locator = Locator { unLocator :: Text }

createLocator :: Text -> Int -> Locator
createLocator projectName organizationId = Locator $ "custom+" <> pack (show organizationId) <> "/" <> projectName

newtype RevisionLocator = RevisionLocator { unRevisionLocator :: Text }

createRevisionLocator :: Text -> Int -> Text -> RevisionLocator
createRevisionLocator projectName organizationId revision = do
  let locator = createLocator projectName organizationId
  RevisionLocator $ unLocator locator <> "$" <> revision

-- /api/vendored-package-scan/sherlock-info
sherlockInfoEndpoint :: Url 'Https -> Url 'Https
sherlockInfoEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "sherlock-info"

-- /api/vendored-package-scan/ci
createProjectEndpoint :: Url 'Https -> Url 'Https
createProjectEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "ci"

-- /api/vendored-package-scan/ci/complete
completeProjectEndpoint :: Url 'Https -> Url 'Https
completeProjectEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "ci" /: "complete"

-- /api/vendored-package-scan/project-scan-filters/:locator
projectScanFiltersEndpoint :: Url 'Https -> Locator -> Url 'Https
projectScanFiltersEndpoint baseurl locator = baseurl /: "api" /: "vendored-package-scan" /: "project-scan-filters" /: unLocator locator

{-
  FIXME: Every function below this line is using a data structure designed for the CLI.
  This tightly couples us to our CLI API, and is very tedious to change with the merge of `vpscli` and `fossa` exe's.
-}
createCoreProject :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> Text -> FossaOpts -> m ()
createCoreProject name revision FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey
  let body = object ["name" .= name, "revision" .= revision]

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (createProjectEndpoint baseUrl) (ReqBodyJson body) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

completeCoreProject :: (Has (Lift IO) sig m, Has Diagnostics sig m) => RevisionLocator -> FossaOpts -> m ()
completeCoreProject locator FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey
  let body = object ["locator" .= unRevisionLocator locator]

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (completeProjectEndpoint baseUrl) (ReqBodyJson body) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

getSherlockInfo :: (Has (Lift IO) sig m, Has Diagnostics sig m) => FossaOpts -> m SherlockInfo
getSherlockInfo FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req GET (sherlockInfoEndpoint baseUrl) NoReqBody jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

getProjectScanFilters :: (Has (Lift IO) sig m, Has Diagnostics sig m) => FossaOpts -> Locator -> m FilterExpressions
getProjectScanFilters FossaOpts{..} locator = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req GET (projectScanFiltersEndpoint baseUrl locator) NoReqBody jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

overrideScanFilters :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m) => VPSOpts -> Locator -> m (VPSOpts, Bool)
overrideScanFilters vpsOpts@VPSOpts { fileFilter = (FilterExpressions []) } locator = do
  let VPSOpts{..} = vpsOpts
  logDebug "[All] Fetching scan file filter from FOSSA"
  overrideFilters <- getProjectScanFilters fossa locator
  logDebug $ pretty $ "[All] Using scan file filter: " <> encodeFilterExpressions overrideFilters
  pure (vpsOpts{fileFilter = overrideFilters}, True)
overrideScanFilters vpsOpts _ = do
  logDebug "[All] Scan file filters provided locally"
  pure (vpsOpts, False)
  
storeUpdatedScanFilters :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m) => Locator -> FilterExpressions -> FossaOpts -> m ()
storeUpdatedScanFilters _ (FilterExpressions []) _ = do
  logDebug "[All] No scan file filter was set, skipping update"
  pure ()
storeUpdatedScanFilters locator filters FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey
  logDebug "[All] Updating FOSSA with new scan file filter for this project"
  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (projectScanFiltersEndpoint baseUrl locator) (ReqBodyJson filters) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()  
