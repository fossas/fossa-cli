{-# LANGUAGE GADTs #-}

module Control.Carrier.DockerEngineApi (runDockerEngineApi) where

import Conduit (runConduit, runResourceT, (.|))
import Control.Carrier.Diagnostics (errorBoundaryIO)
import Control.Carrier.Simple (Has, SimpleC, interpret)
import Control.Effect.Diagnostics (fatalText, fromMaybeText)
import Control.Effect.Diagnostics qualified as Diag (Diagnostics)
import Control.Effect.DockerEngineApi (DockerEngineApiF (ExportImage, GetImageSize, IsDockerEngineAccessible))
import Control.Effect.Lift (Lift, sendIO)
import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import Data.Conduit.Binary (sinkFile)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Diag.Result (resultToMaybe)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Internal (Connection)
import Network.HTTP.Conduit qualified as HTTPConduit
import Network.HTTP.Types (ok200)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import Path (Abs, File, Path)

type DockerEngineApiC = SimpleC DockerEngineApiF

runDockerEngineApi :: (Has (Lift IO) sig m, Has Diag.Diagnostics sig m) => Text -> DockerEngineApiC m a -> m a
runDockerEngineApi socketHost = do
  interpret $ \case
    ExportImage img path -> exportDockerImage socketHost img path
    GetImageSize img -> getDockerImageSize socketHost img
    IsDockerEngineAccessible -> isDockerEngineAccessible socketHost

-- | Exports Docker Image to a given path.
-- Refer to: https://docs.docker.com/engine/api/v1.28/#tag/Image/operation/ImageGet
exportDockerImage :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => Text -> Text -> Path Abs File -> m ()
exportDockerImage socketHost img sinkTarget = do
  let request = HTTP.parseRequest_ $ baseApi <> "images/" <> (toString img) <> "/get"
  manager <- sendIO $ dockerClient socketHost
  -- Performs equivalent of for given image:
  -- >> curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/images/redis:alpine/get" > img.tar
  sendIO . runResourceT $ do
    response <- HTTPConduit.http request manager
    runConduit $ HTTPConduit.responseBody response .| sinkFile (toString sinkTarget)

-- | Gets Image Size from Image Description Json
-- Refer to: https://docs.docker.com/engine/api/v1.28/#tag/Image/operation/ImageInspect
getDockerImageSize :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => Text -> Text -> m Int
getDockerImageSize socketHost img = do
  let request = HTTP.parseRequest_ $ baseApi <> "images/" <> (toString img) <> "/json"

  -- Performs equivalent of for given image:
  -- >> curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/images/redis:alpine/json"
  response <- sendIO $ HTTP.httpLbs request =<< dockerClient socketHost

  let body = HTTP.responseBody response
  case eitherDecode body of
    Left err -> fatalText . toText $ err
    Right (DockerImageInspectJson imgSize) -> pure imgSize

-- | True if Docker Engine API is accessible, otherwise False.
-- Refer to: https://docs.docker.com/engine/api/v1.28/#tag/System/operation/SystemPing
isDockerEngineAccessible :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => Text -> m Bool
isDockerEngineAccessible socketHost = do
  -- Performs equivalent of for given image:
  -- >> curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/_ping"

  -- `Network.Socket.connect` throws async IO exception, only when socket does not
  -- exist at /var/run/docker.sock location, If daemon is running but refusing connection,
  -- then we receive nominal error code (as expected).
  response <- errorBoundaryIO $ sendIO (HTTP.httpLbs (HTTP.parseRequest_ $ baseApi <> "_ping") =<< dockerClient socketHost)
  response' <- fromMaybeText ("Could not connect to docker daemon at: " <> toText socketHost) $ resultToMaybe response
  pure $ (HTTP.responseStatus response') == ok200

newtype DockerImageInspectJson = DockerImageInspectJson {imageSize :: Int}
  deriving (Eq, Ord, Show)

instance FromJSON DockerImageInspectJson where
  parseJSON = withObject "image inspect json" $ \o ->
    DockerImageInspectJson <$> o .: "Size"

baseApi :: String
baseApi = "http://localhost/v1.28/"

dockerClient :: Has (Lift IO) sig m => Text -> m HTTP.Manager
dockerClient = unixSocketClient . toString

unixSocketClient :: Has (Lift IO) sig m => FilePath -> m HTTP.Manager
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
