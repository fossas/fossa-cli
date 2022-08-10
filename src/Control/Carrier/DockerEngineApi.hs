{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Control.Carrier.DockerEngineApi (runDockerEngineApi) where

import Conduit (runConduit, runResourceT, (.|))
import Control.Carrier.Simple (Has, SimpleC, interpret)
import Control.Effect.Diagnostics (fatalText)
import Control.Effect.Diagnostics qualified as Diag (Diagnostics)
import Control.Effect.DockerEngineApi (DockerEngineApiF (ExportImage, GetImageSize, IsAccessible))
import Control.Effect.Lift (Lift, sendIO)
import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import Data.Conduit.Binary (sinkFile)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Internal (Connection)
import Network.HTTP.Conduit qualified as HTTPConduit
import Network.HTTP.Types (Status (statusCode))
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import Path (Abs, File, Path)

type DockerEngineApiC = SimpleC DockerEngineApiF

runDockerEngineApi :: (Has (Lift IO) sig m, Has Diag.Diagnostics sig m) => DockerEngineApiC m a -> m a
runDockerEngineApi = interpret $ \case
  ExportImage img path -> exportDockerImage img path
  GetImageSize img -> getDockerImageSize img
  IsAccessible -> isAccessible

-- | Exports Docker Image to a given path.
-- Refer to: https://docs.docker.com/engine/api/v1.41/#tag/Image/operation/ImageGet
exportDockerImage :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => Text -> Path Abs File -> m ()
exportDockerImage img sinkTarget = do
  let request = HTTP.parseRequest_ $ baseApi <> "images/" <> (toString img) <> "/get"
  manager <- sendIO dockerClient
  -- Performs equivalent of for given image:
  -- >> curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.41/images/redis:alpine/get" > img.tar
  sendIO . runResourceT $ do
    response <- HTTPConduit.http request manager
    runConduit $ HTTPConduit.responseBody response .| sinkFile (toString sinkTarget)

-- | Gets Image Size from Image Description Json
-- Refer to: https://docs.docker.com/engine/api/v1.41/#tag/Image/operation/ImageInspect
getDockerImageSize :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => Text -> m Int
getDockerImageSize img = do
  let request = HTTP.parseRequest_ $ baseApi <> "images/" <> (toString img) <> "/json"

  -- Performs equivalent of for given image:
  -- >> curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.41/images/redis:alpine/json"
  response <- sendIO $ HTTP.httpLbs request =<< dockerClient

  let body = HTTP.responseBody response
  case eitherDecode body of
    Left err -> fatalText . toText $ err
    Right (DockerImageInspectJson imgSize) -> pure imgSize

-- | True if Docker Engine API is accessible, otherwise False.
-- Refer to: https://docs.docker.com/engine/api/v1.41/#tag/System/operation/SystemPing
isAccessible :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => m Bool
isAccessible = do
  -- Performs equivalent of for given image:
  -- >> curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.41/_ping"
  response <- sendIO $ HTTP.httpLbs (HTTP.parseRequest_ $ baseApi <> "_ping") =<< dockerClient
  pure $ statusCode (HTTP.responseStatus response) == 200

newtype DockerImageInspectJson = DockerImageInspectJson {imageSize :: Int}
  deriving (Eq, Ord, Show)

instance FromJSON DockerImageInspectJson where
  parseJSON = withObject "image inspect json" $ \o ->
    DockerImageInspectJson <$> o .: "Size"

baseApi :: String
baseApi = "http://localhost/v1.28/"

dockerClient :: Has (Lift IO) sig m => m HTTP.Manager
dockerClient = unixSocketClient "/var/run/docker.sock"

unixSocketClient :: FilePath -> Has (Lift IO) sig m => m HTTP.Manager
unixSocketClient socketPath = socketHttpManager
  where
    socketHttpManager :: Has (Lift IO) sig m => m HTTP.Manager
    socketHttpManager = sendIO $ HTTP.newManager socketManagerSettings

    socketManagerSettings :: HTTP.ManagerSettings
    socketManagerSettings = HTTP.defaultManagerSettings{HTTP.managerRawConnection = pure open}

    open :: Maybe Socket.HostAddress -> String -> Int -> IO Connection
    open _ _ _ = do
      sock <-
        Socket.socket
          Socket.AF_UNIX -- We want unix domain socket
          Socket.Stream -- We want TCP based connection
          Socket.defaultProtocol -- Default of 0 (will defer based on socket type)
      Socket.connect sock (Socket.SockAddrUnix socketPath)
      HTTP.makeConnection
        (SocketByteString.recv sock chunkBytes)
        (SocketByteString.sendAll sock)
        (Socket.close sock)

    chunkBytes :: Int
    chunkBytes = 4096
