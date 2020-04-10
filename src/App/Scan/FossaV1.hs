module App.Scan.FossaV1
  ( uploadAnalysis
  , UploadResponse(..)
  , FossaError(..)
  ) where

import App.Scan.Project
import Control.Carrier.Error.Either
import Data.List (isInfixOf)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import qualified Network.HTTP.Types as HTTP
import Prologue
import Srclib.Converter (toSourceUnit)
import Srclib.Types

-- TODO: git commit?
cliVersion :: Text
cliVersion = "spectrometer"

uploadUrl :: Url 'Https
uploadUrl = https "app.fossa.com" /: "api" /: "builds" /: "custom"

newtype FossaReq m a = FossaReq { runFossaReq :: ErrorC FossaError m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Effect sig, MonadIO m) => MonadHttp (FossaReq m) where
  handleHttpException = FossaReq . throwError . mangleError

data UploadResponse = UploadResponse
  { uploadLocator :: Text
  , uploadError   :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UploadResponse where
  parseJSON = withObject "UploadResponse" $ \obj ->
    UploadResponse <$> obj .: "locator"
                   <*> obj .:? "error"

data FossaError
  = InvalidProjectOrRevision
  | NoPermission
  | JsonDeserializeError String
  | OtherError HttpException
  deriving (Show, Generic)

uploadAnalysis
  :: Text -- api key
  -> Text -- project name
  -> Text -- project revision
  -> [Project]
  -> IO (Either FossaError UploadResponse)
uploadAnalysis key name revision projects = runError . runFossaReq $ do
  let filteredProjects = filter (isProductionPath . projectPath) projects
      sourceUnits = fromMaybe [] $ traverse toSourceUnit filteredProjects
      opts = "locator" =: renderLocator (Locator "custom" name (Just revision))
          <> "v" =: cliVersion
          <> "managedBuild" =: True
          <> "title" =: name
          <> header "Authorization" ("token " <> encodeUtf8 key)
  resp <- req POST uploadUrl (ReqBodyJson sourceUnits) jsonResponse opts
  pure (responseBody resp)

mangleError :: HttpException -> FossaError
mangleError err = case err of
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
    case HTTP.responseStatus resp of
      HTTP.Status 429 _ -> InvalidProjectOrRevision
      HTTP.Status 403 _ -> NoPermission
      _                 -> OtherError err
  JsonHttpException msg -> JsonDeserializeError msg
  _ -> OtherError err

-- we specifically want Rel paths here: parent directories shouldn't affect path
-- filtering
isProductionPath :: Path Rel fd -> Bool
isProductionPath path = not $ any (`isInfixOf` toFilePath path)
  [ "doc/"
  , "docs/"
  , "test/"
  , "example/"
  , "examples/"
  , "vendor/"
  , "node_modules/"
  , ".srclib-cache/"
  , "spec/"
  , "Godeps/"
  , ".git/"
  , "bower_components/"
  , "third_party/"
  , "third-party/"
  , "tmp/"
  , "Carthage/"
  , "Checkouts/"
  ]
