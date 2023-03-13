{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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
  NotSupportedManifestFmt (NotSupportedManifestFmt),
  OciManifestV2,
  blobEntries,
  supportedManifestKinds,
  toDockerManifest,
 )
import Container.Docker.OciManifestIndex (digestOf, supportedManifestIndexKinds)
import Container.Docker.SourceParser (
  RegistryImageSource (
    RegistryImageSource,
    platformArchitecture,
    registryContainerRepositoryReference,
    registryCred
  ),
  RepoDigest,
  RepoReference (RepoReferenceDigest),
 )
import Control.Algebra (Has)
import Control.Carrier.AtomicCounter (runAtomicCounter)
import Control.Carrier.ContainerRegistryApi.Authorization (applyAuthToken, getAuthToken, mkRequest)
import Control.Carrier.ContainerRegistryApi.Common (
  RegistryCtx (RegistryCtx),
  fromResponse,
  getContentType,
  getToken,
 )

import Control.Carrier.Finally (runFinally)
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Carrier.StickyLogger (runStickyLogger)
import Control.Carrier.TaskPool (withTaskPool)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.STM (newEmptyTMVarIO)
import Control.Effect.ContainerRegistryApi (
  ContainerRegistryApiF (ExportImage, GetImageManifest),
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  fatal,
  fatalText,
  fromEither,
  fromMaybeText,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (Reader)
import Control.Effect.TaskPool (forkTask)
import Control.Monad (when)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString (ByteString, writeFile)
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.Conduit.Zlib (ungzip)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (
  LazyStrict (toStrict),
  toString,
  toText,
 )
import Data.Text (Text)
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
  newManager,
  parseRequest,
 )
import Network.HTTP.Conduit (tlsManagerSettings)
import Network.HTTP.Conduit qualified as HTTPConduit
import Network.HTTP.Types.Header (ResponseHeaders)
import Path (Abs, Dir, File, Path, filename, mkRelFile, toFilePath, (</>))
import Path.Internal (Path (..))

-- | A carrier to run Registry API functions in the IO monad
type ContainerRegistryApiC m = SimpleC ContainerRegistryApiF (ReaderC RegistryCtx m)

-- | Runs ContainerRegistryAPI effects as IO operations
runContainerRegistryApi ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  ContainerRegistryApiC m b ->
  m b
runContainerRegistryApi act = do
  registryCtx <- sendIO $ RegistryCtx <$> newEmptyTMVarIO
  runContainerRegistryApi' registryCtx act

runContainerRegistryApi' ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  RegistryCtx ->
  ContainerRegistryApiC m a ->
  m a
runContainerRegistryApi' initCtx =
  do
    (runReader (initCtx :: RegistryCtx))
    . interpret
      ( \case
          GetImageManifest imgSrc -> getImageManifest imgSrc
          ExportImage imgSrc dir -> exportImage imgSrc dir
      )

reqManager :: Has (Lift IO) sig m => m Manager
reqManager = sendIO $ newManager tlsManagerSettings

-- | Manifest Endpoint.
-- Refer to: https://github.com/opencontainers/distribution-spec/blob/main/spec.md#pulling-manifests
manifestEndpoint :: Has (Lift IO) sig m => RegistryImageSource -> m Request
manifestEndpoint (RegistryImageSource url scheme _ repo ref _) =
  sendIO $ parseRequest (toString $ (toText . show $ scheme) <> url <> "/v2/" <> repo <> "/manifests/" <> (toText . show $ ref))

-- | Blob Endpoint.
-- Refer to: https://github.com/opencontainers/distribution-spec/blob/main/spec.md#pulling-blobs
blobEndpoint :: Has (Lift IO) sig m => RegistryImageSource -> m Request
blobEndpoint (RegistryImageSource url scheme _ repo ref _) =
  sendIO $ parseRequest (toString $ (toText . show $ scheme) <> url <> "/v2/" <> repo <> "/blobs/" <> (toText . show $ ref))

-- | Retrieve Manifest.
getImageManifest ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  , Has (Reader RegistryCtx) sig m
  ) =>
  RegistryImageSource ->
  m OciManifestV2
getImageManifest src = context "Getting Image Manifest" $ do
  manager <- reqManager
  resp <-
    fromResponse
      =<< mkRequest manager (registryCred src) (Just supportedManifestKinds)
      =<< manifestEndpoint src

  let respBody :: ByteStringLazy.ByteString
      respBody = responseBody resp

  if isManifestIndex (responseHeaders resp)
    then do
      manifestIndex <- fromEither $ eitherDecode respBody
      logDebug "Retrieved multi-platform image manifest index."

      let platformArch :: Text
          platformArch = platformArchitecture src

      logDebug . pretty $ "Looking for platform architecture: " <> platformArch
      manifestDigest <-
        fromMaybeText
          ("Could not find manifest for platform architecture of: " <> platformArch)
          (digestOf manifestIndex platformArch)

      parseOciManifest
        =<< mkRequest manager (registryCred src) (Just supportedManifestKinds)
        =<< (manifestEndpoint $ src{registryContainerRepositoryReference = manifestDigest})
    else do
      logDebug "Retrieved single-platform image manifest."
      parseOciManifest resp
  where
    isSupportedManifestKind :: ResponseHeaders -> Bool
    isSupportedManifestKind headers =
      any (\t -> Just t == getContentType headers) supportedManifestKinds

    isManifestIndex :: ResponseHeaders -> Bool
    isManifestIndex headers =
      any (\t -> Just t == getContentType headers) supportedManifestIndexKinds

    parseOciManifest :: Has Diagnostics sig m => Response ByteStringLazy.ByteString -> m OciManifestV2
    parseOciManifest resp =
      if isSupportedManifestKind (responseHeaders resp)
        then case eitherDecode (responseBody resp) of
          Left err -> fatalText $ toText err
          Right manifest -> pure manifest
        else
          fatal $
            NotSupportedManifestFmt
              (fromMaybe "<Unknown-Content-Type>" $ getContentType . responseHeaders $ resp)
              src

-- | Downloads Image into specific directory, and returns tarball.
exportImage ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has (Reader RegistryCtx) sig m
  ) =>
  RegistryImageSource ->
  Path Abs Dir ->
  m (Path Abs File)
exportImage imgSrc dir = context "Exporting Image" $ do
  manager <- reqManager
  manifest <- getImageManifest imgSrc
  let blobs = blobEntries manifest

  capabilities <- max 2 <$> sendIO getNumCapabilities
  runStickyLogger SevInfo . runFinally
    $ context "Downloading image artifact from registry"
      . withTaskPool capabilities (updateProgress)
      . runAtomicCounter
    $ do
      traverse (forkTask . exportBlob manager imgSrc dir) blobs

  mkTarball dir manifest imgSrc

-- | Exports a Blob to a directory.
exportBlob ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader RegistryCtx) sig m
  ) =>
  Manager ->
  RegistryImageSource ->
  Path Abs Dir ->
  (RepoDigest, Bool, Text) ->
  m (Path Abs File)
exportBlob manager imgSrc dir (digest, isGzip, targetFilename) = do
  ctx <- ask
  let sinkTarget :: Path Abs File
      sinkTarget = dir </> Path (toString targetFilename)

  let imgSrc' = imgSrc{registryContainerRepositoryReference = RepoReferenceDigest digest}

  -- Prepare request with necessary authorization
  req <- blobEndpoint imgSrc'
  token <- getAuthToken (registryCred imgSrc) req manager =<< getToken ctx
  let req' = applyAuthToken token req

  -- Download image artifact
  sendIO . runResourceT $ do
    response <- HTTPConduit.http req' manager
    runConduit $
      HTTPConduit.responseBody response
        .| (if isGzip then ungzip else idC)
        .| sinkFile (toString sinkTarget)

  logInfo . pretty $
    if isGzip
      then "Gzip extracted & downloaded: " <> targetFilename
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
mkTarball dir manifest imgSrc = context "Making image tarball" $ do
  let tarballFile :: Path Abs File = dir </> $(mkRelFile "image.tar")
  let manifestFile :: Path Abs File = dir </> $(mkRelFile $ toString manifestFilename)

  sendIO
    $ Data.ByteString.writeFile (toFilePath manifestFile)
    $ toStrict
      . encode
    $ toDockerManifest manifest imgSrc

  files <- snd <$> listDir dir

  when (null files) $
    fatalText $
      "Directory " <> toText (show dir) <> " cannot be made into a image tarball: it does not contain any files!"

  context "Creating tarball" $
    sendIO $
      Tar.create
        (toFilePath tarballFile) -- Location where to store tarball file.
        (toFilePath dir) -- Base Directory.
        (map (toFilePath . filename) files) -- Filepath as seen from Base directory in tarball.
  pure tarballFile
