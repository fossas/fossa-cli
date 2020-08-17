module App.VPSScan.Scan.Core
  ( coreAuthHeader
  , createCoreProject
  , completeCoreProject
  , getSherlockInfo
  , createLocator
  , createRevisionLocator
  , buildRevision
  , Locator(..)
  , SherlockInfo(..)
  )
where

import App.VPSScan.Types
import App.Util (parseUri)
import Data.Text (pack, Text)
import Prelude
import Network.HTTP.Req
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Effect.Diagnostics
import Data.Aeson
import Data.Time.Clock.POSIX (getPOSIXTime)

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

buildRevision :: (MonadIO m) => Maybe Text -> m Text
buildRevision (Just userProvidedRevision) = pure (userProvidedRevision)
buildRevision Nothing = do
  posixTime <- liftIO getPOSIXTime
  pure (pack $ show $ (floor $ toRational posixTime :: Int))

newtype Locator = Locator { unLocator :: Text }

createLocator :: Text -> Int -> Locator
createLocator projectName organizationId = Locator $ "custom+" <> (pack $ show organizationId) <> "/" <> projectName

createRevisionLocator :: Text -> Int -> Text -> Locator
createRevisionLocator projectName organizationId revision = do
  let locator = createLocator projectName organizationId
  Locator $ unLocator locator <> "$" <> revision

-- /api/vendored-package-scan/sherlock-info
sherlockInfoEndpoint :: Url 'Https -> Url 'Https
sherlockInfoEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "sherlock-info"

-- /api/vendored-package-scan/ci
createProjectEndpoint :: Url 'Https -> Url 'Https
createProjectEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "ci"

-- /api/vendored-package-scan/ci/complete
completeProjectEndpoint :: Url 'Https -> Url 'Https
completeProjectEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "ci" /: "complete"

createCoreProject :: (MonadIO m, Has Diagnostics sig m) => Text -> Text -> FossaOpts -> m ()
createCoreProject name revision FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey
  let body = object ["name" .= name, "revision" .= revision]

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (createProjectEndpoint baseUrl) (ReqBodyJson body) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

completeCoreProject :: (MonadIO m, Has Diagnostics sig m) => Text -> FossaOpts -> m ()
completeCoreProject locator FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey
  let body = object ["locator" .= locator]

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (completeProjectEndpoint baseUrl) (ReqBodyJson body) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

getSherlockInfo :: (MonadIO m, Has Diagnostics sig m) => FossaOpts -> m SherlockInfo
getSherlockInfo FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req GET (sherlockInfoEndpoint baseUrl) NoReqBody jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)
