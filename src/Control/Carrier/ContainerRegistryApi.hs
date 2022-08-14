{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Carrier.ContainerRegistryApi (
  runContainerRegistryApi,
  ContainerRegistryApiC,
) where

import App.Fossa.Analyze (updateProgress)
import Codec.Archive.Tar qualified as Tar
import Conduit (
  ConduitT,
  PrimMonad,
  awaitForever,
  runConduit,
  runResourceT,
  sinkFile,
  yield,
  (.|),
 )
import Container.Docker.Manifest (manifestFilename)
import Container.Docker.OciManifest (
  OciManifestV2,
  blobEntries,
  digestOf,
  supportedManifestKinds,
  supportedManifestListKinds,
  toDockerManifest,
 )
import Container.Docker.SourceParser (
  RegistryImageSource (
    RegistryImageSource,
    platformArchitecture,
    registryContainerRepositoryReference,
    registryCred
  ),
 )
import Control.Carrier.AtomicCounter (runAtomicCounter)
import Control.Carrier.ContainerRegistryApi.Authorization (getAuthToken, getAuthToken')
import Control.Carrier.ContainerRegistryApi.Common (
  acceptsContentType,
  fromResponse,
  getContentType,
  logHttp,
 )
import Control.Carrier.Finally (runFinally)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Carrier.StickyLogger (runStickyLogger)
import Control.Carrier.TaskPool (withTaskPool)
import Control.Concurrent (getNumCapabilities)
import Control.Effect.ContainerRegistryApi (
  ContainerRegistryApiF (..),
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  fatalText,
  fromEither,
  fromMaybeText,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.TaskPool (forkTask)
import Control.Monad (when)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString (ByteString, writeFile)
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.Conduit.Zlib (ungzip)
import Data.String.Conversion (
  LazyStrict (toStrict),
  encodeUtf8,
  toString,
  toText,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Logger (
  Logger,
  Pretty (pretty),
  Severity (SevInfo),
  logDebug,
  logInfo,
 )
import Effect.ReadFS (ReadFS, listDir)
import Network.HTTP.Client (
  Manager,
  Request,
  Response (responseBody, responseHeaders),
  applyBearerAuth,
  newManager,
  parseRequest,
 )
import Network.HTTP.Conduit (tlsManagerSettings)
import Network.HTTP.Conduit qualified as HTTPConduit
import Network.HTTP.Types.Header (ResponseHeaders)
import Path (Abs, Dir, File, Path, filename, mkRelFile, toFilePath, (</>))
import Path.Internal (Path (..))

type ContainerRegistryApiC = SimpleC ContainerRegistryApiF

runContainerRegistryApi ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  ContainerRegistryApiC m a ->
  m a
runContainerRegistryApi = interpret $ \case
  GetImageManifest imageSrc -> getImageManifest imageSrc
  ExportImage imageSrc dir -> exportImage imageSrc dir

reqManager :: Has (Lift IO) sig m => m Manager
reqManager = sendIO $ newManager tlsManagerSettings

-- | Manifest Endpoint.
-- Refer to: https://github.com/opencontainers/distribution-spec/blob/main/spec.md#pulling-manifests
manifestEndpoint :: Has (Lift IO) sig m => RegistryImageSource -> m Request
manifestEndpoint (RegistryImageSource url scheme _ repo ref _) =
  sendIO $ parseRequest (toString $ (toText . show $ scheme) <> url <> "/v2/" <> repo <> "/manifests/" <> ref)

-- | Blob Endpoint.
-- Refer to: https://github.com/opencontainers/distribution-spec/blob/main/spec.md#pulling-blobs
blobEndpoint :: Has (Lift IO) sig m => RegistryImageSource -> m Request
blobEndpoint (RegistryImageSource url scheme _ repo ref _) =
  sendIO $ parseRequest (toString $ (toText . show $ scheme) <> url <> "/v2/" <> repo <> "/blobs/" <> ref)

-- | Retrieve Manifest.
getImageManifest ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  ) =>
  RegistryImageSource ->
  m OciManifestV2
getImageManifest imgSrc = context "Getting Image Manifest" $ do
  resp <- fromResponse =<< mkRequest (registryCred imgSrc) (Just supportedManifestKinds) =<< manifestEndpoint imgSrc

  let respBody :: ByteStringLazy.ByteString
      respBody = responseBody resp

  if isManifestList (responseHeaders resp)
    then do
      manifestIndex <- fromEither $ eitherDecode respBody
      logDebug "Retrieved image manifest list, This is multi-platform image."

      let platformArch :: Text
          platformArch = platformArchitecture imgSrc

      logDebug . pretty $ "Looking for platform architecture: " <> platformArch
      manifestDigest <-
        fromMaybeText
          ("Could not find manifest for platform architecture of: " <> platformArch)
          (digestOf manifestIndex platformArch)

      resp' <-
        mkRequest Nothing (Just supportedManifestKinds)
          =<< (manifestEndpoint $ imgSrc{registryContainerRepositoryReference = manifestDigest})

      parseOciManifest (responseBody resp')
    else parseOciManifest respBody
  where
    isManifestList :: ResponseHeaders -> Bool
    isManifestList headers = any (\t -> Just t == getContentType headers) supportedManifestListKinds

    parseOciManifest :: Has Diagnostics sig m => ByteStringLazy.ByteString -> m OciManifestV2
    parseOciManifest respBody = case eitherDecode respBody of
      Left err -> fatalText $ toText err
      Right manifest -> pure manifest

-- | Downloads Image into specific directory, and returns tarball.
exportImage ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  RegistryImageSource ->
  Path Abs Dir ->
  m (Path Abs File)
exportImage imgSrc dir = context "Exporting Image" $ do
  manager <- reqManager
  manifest <- getImageManifest imgSrc

  let blobs = blobEntries manifest
  capabilities <- sendIO getNumCapabilities
  runStickyLogger SevInfo . runFinally
    $ context "Downloading Image Artifact from Registry"
      . withTaskPool capabilities (updateProgress)
      . runAtomicCounter
    $ do
      traverse (forkTask . exportBlob manager imgSrc dir) blobs

  mkTarball dir manifest imgSrc

-- | Makes a request.
mkRequest ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Maybe (Text, Text) -> -- Credentials to use when retrieving authorization token
  Maybe [Text] -> -- Content-Type to request
  Request -> -- Request to make
  m (Response ByteStringLazy.ByteString)
mkRequest creds accepts req = do
  manager <- reqManager
  token <- context "Getting Authorization Token" $ getAuthToken creds req manager

  let req' = case token of
        Nothing -> req
        Just token' -> applyBearerAuth (encodeUtf8 token') req

  let req'' = case accepts of
        Just a -> req' `acceptsContentType` (Text.intercalate ", " a)
        Nothing -> req'

  logHttp req'' manager

-- | Exports a Blob to a directory.
exportBlob ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Manager ->
  RegistryImageSource ->
  Path Abs Dir ->
  (Text, Bool, Text) ->
  m (Path Abs File)
exportBlob manager imgSrc dir (digest, isGzip, targetFilename) = do
  let sinkTarget :: Path Abs File
      sinkTarget = dir </> Path (toString targetFilename)

  let imgSrc' = imgSrc{registryContainerRepositoryReference = digest}
  req <- blobEndpoint imgSrc'

  token <- getAuthToken' False (registryCred imgSrc) req manager

  let req' :: Request
      req' = case token of
        Nothing -> req
        Just token' -> applyBearerAuth (encodeUtf8 token') req

  sendIO . runResourceT $ do
    response <- HTTPConduit.http req' manager
    runConduit $
      HTTPConduit.responseBody response
        .| (if isGzip then ungzip else idC)
        .| sinkFile (toString sinkTarget)

  logInfo . pretty $
    if isGzip
      then "Gzip Extracted & Downloaded: " <> targetFilename
      else "Downloaded: " <> targetFilename

  pure sinkTarget

-- | Identity Conduit
idC :: (PrimMonad m) => ConduitT ByteString ByteString m ()
idC = awaitForever yield

-- | Creates Tarball in Directory for given files.
mkTarball ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  OciManifestV2 ->
  RegistryImageSource ->
  m (Path Abs File)
mkTarball dir manifest (RegistryImageSource _ _ _ repo ref _) = context "Making Image Tarball" $ do
  let tarballFile :: Path Abs File = dir </> $(mkRelFile "image.tar")
  let manifestFile :: Path Abs File = dir </> $(mkRelFile $ toString manifestFilename)

  sendIO
    $ Data.ByteString.writeFile (toFilePath manifestFile)
    $ toStrict
      . encode
    $ toDockerManifest manifest repo ref

  files <- snd <$> listDir dir

  when (null files) $
    fatalText "Expected to have some files in the directory for OCI archiving!"

  context "Creating Tarball" $
    sendIO $
      Tar.create
        (toFilePath tarballFile) -- Location where to store tarball file.
        (toFilePath dir) -- Base Directory.
        (map (toFilePath . filename) files) -- Filepath as seen from Base directory in tarball.
  pure tarballFile
