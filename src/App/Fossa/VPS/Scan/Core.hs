{-# LANGUAGE DataKinds #-}

module App.Fossa.VPS.Scan.Core (
  createLocator,
  createRevisionLocator,
  getSherlockInfo,
  Locator (..),
  RevisionLocator (..),
  SherlockInfo (..),
) where

import App.Fossa.VPS.Types
import Control.Carrier.Trace.Printing
import Control.Effect.Diagnostics
import Control.Effect.Lift (Lift)
import Data.Aeson
import Data.String.Conversion (toText)
import Data.Text (Text)
import Fossa.API.Types (ApiOpts (..), useApiOpts)
import Network.HTTP.Req
import Prelude

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

newtype Locator = Locator {unLocator :: Text}

createLocator :: Text -> Int -> Locator
createLocator projectName organizationId = Locator $ "custom+" <> toText (show organizationId) <> "/" <> projectName

newtype RevisionLocator = RevisionLocator {unRevisionLocator :: Text}

createRevisionLocator :: Text -> Int -> Text -> RevisionLocator
createRevisionLocator projectName organizationId revision = do
  let locator = createLocator projectName organizationId
  RevisionLocator $ unLocator locator <> "$" <> revision

-- /api/vendored-package-scan/sherlock-info
sherlockInfoEndpoint :: Url 'Https -> Url 'Https
sherlockInfoEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "sherlock-info"

getSherlockInfo :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> m SherlockInfo
getSherlockInfo apiOpts = runHTTP $ do
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  resp <- req GET (sherlockInfoEndpoint baseUrl) NoReqBody jsonResponse (baseOptions <> header "Content-Type" "application/json")
  pure (responseBody resp)
