{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container (
  ImageText (..),
  imageTextArg,
  Locator (..),
  SyftResponse (..),
  ResponseArtifact (..),
  ResponseSource (..),
  ResponseDistro (..),
  SourceTarget (..),
  ContainerScan (..),
  ContainerImage (..),
  ContainerArtifact (..),
  runSyft,
  toContainerScan,
  extractRevision,
  parseSyftOutput,
  parseSyftOutputMain,
  dumpSyftScanMain,
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toExecutablePath, withSyftBinary)
import App.Types (OverrideProject (..), ProjectRevision (..))
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Extra ((<$$>))
import Data.List (nub)
import Data.Map.Lazy qualified as LMap
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text, pack)
import Data.Text.Extra (breakOnAndRemove)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson, execThrow, runExecIO)
import Effect.Logger
import Effect.ReadFS (ReadFS, ReadFSIOC (runReadFSIO), readContentsJson, resolveFile)
import Options.Applicative (Parser, argument, help, metavar, str)
import Path (Dir, Rel, reldir, toFilePath)
import Path.IO (getCurrentDir)

newtype ImageText = ImageText {unImageText :: Text} deriving (Show, Eq, Ord)

imageTextArg :: Parser ImageText
imageTextArg = ImageText . pack <$> argument str (metavar "IMAGE" <> help "The image to scan")

newtype Locator = Locator {unLocator :: Text} deriving (Eq, Ord, Show)

-- | The output of the syft binary
data SyftResponse = SyftResponse
  { responseArtifacts :: [ResponseArtifact]
  , responseSource :: ResponseSource
  , responseDistro :: ResponseDistro
  }

instance FromJSON SyftResponse where
  parseJSON = withObject "SyftResponse" $ \obj ->
    SyftResponse <$> (filter artifactTypeIsOk <$> obj .: "artifacts")
      <*> obj .: "source"
      <*> obj .: "distro"

artifactTypeIsOk :: ResponseArtifact -> Bool
artifactTypeIsOk art = artifactType art `elem` acceptedArtifactTypes

acceptedArtifactTypes :: [Text]
acceptedArtifactTypes = ["deb", "rpm", "apk"]

data ResponseArtifact = ResponseArtifact
  { artifactName :: Text
  , artifactVersion :: Text
  , artifactType :: Text
  , artifactLocations :: [ContainerLocation]
  , artifactPkgUrl :: Text
  , artifactMetadataType :: Text
  , artifactMetadata :: Maybe (Map Text Value)
  }

instance FromJSON ResponseArtifact where
  parseJSON = withObject "ResponseArtifact" $ \obj ->
    ResponseArtifact <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .: "type"
      <*> obj .: "locations"
      <*> obj .: "purl"
      <*> obj .: "metadataType"
      -- We delete "files" as early as possible, which reduces
      -- the size by over 95% in many cases.
      -- We use Lazy delete to avoid evaluating the innards of
      -- the field, since Aeson will try to avoid evaluating it
      -- as well.
      <*> (LMap.delete "files" <$$> obj .:? "metadata")

newtype ResponseSource = ResponseSource
  {sourceTarget :: SourceTarget}

instance FromJSON ResponseSource where
  parseJSON = withObject "ResponseSource" $ \obj ->
    ResponseSource <$> obj .: "target"

data ResponseDistro = ResponseDistro
  { distroName :: Text
  , distroVersion :: Text
  }

instance FromJSON ResponseDistro where
  parseJSON = withObject "ResponseDistro" $ \obj ->
    ResponseDistro <$> obj .: "name"
      <*> obj .: "version"

data SourceTarget = SourceTarget
  { targetDigest :: Text
  , targetLayers :: [LayerTarget]
  , targetTags :: [Text]
  }

instance FromJSON SourceTarget where
  parseJSON = withObject "SourceTarget" $ \obj ->
    SourceTarget <$> obj .: "imageID"
      <*> obj .: "layers"
      <*> obj .: "tags"

-- Capture container layers from target
-- The digest will correspond to location -> layerId
newtype LayerTarget = LayerTarget {layerTargetDigest :: Text} deriving (Eq, Show, Ord)

instance FromJSON LayerTarget where
  parseJSON = withObject "LayerTarget" $ \obj ->
    LayerTarget <$> obj .: "digest"

instance ToJSON LayerTarget where
  toJSON LayerTarget{..} =
    object ["digest" .= layerTargetDigest]

-- | The reorganized output of syft into a slightly different format
data ContainerScan = ContainerScan
  { imageData :: ContainerImage
  , imageTag :: Text
  , imageDigest :: Text
  }

instance ToJSON ContainerScan where
  toJSON scan = object ["image" .= imageData scan]

data ContainerImage = ContainerImage
  { imageArtifacts :: [ContainerArtifact]
  , imageOs :: Text
  , imageOsRelease :: Text
  , imageLayers :: [LayerTarget]
  }

instance ToJSON ContainerImage where
  toJSON ContainerImage{..} =
    object
      [ "os" .= imageOs
      , "osRelease" .= imageOsRelease
      , "layers" .= imageLayers
      , "artifacts" .= imageArtifacts
      ]

-- Define Layer/Location type to capture layers in which a dep is found
-- omitting "path" from the object to reduce noise
newtype ContainerLocation = ContainerLocation {conLayerId :: Text} deriving (Eq, Show, Ord)

instance FromJSON ContainerLocation where
  parseJSON = withObject "ContainerLocation" $ \obj ->
    ContainerLocation <$> obj .: "layerID"

instance ToJSON ContainerLocation where
  toJSON ContainerLocation{..} =
    object ["layerId" .= conLayerId]

data ContainerArtifact = ContainerArtifact
  { conArtifactName :: Text
  , conArtifactVersion :: Text
  , conArtifactType :: Text
  , conArtifactLocations :: [ContainerLocation]
  , conArtifactPkgUrl :: Text
  , conArtifactMetadataType :: Text
  , conArtifactMetadata :: Map Text Value
  }

instance ToJSON ContainerArtifact where
  toJSON ContainerArtifact{..} =
    object
      [ "name" .= conArtifactName
      , "fullVersion" .= conArtifactVersion
      , "type" .= conArtifactType
      , "locations" .= nub conArtifactLocations
      , "purl" .= conArtifactPkgUrl
      , "metadataType" .= conArtifactMetadataType
      , "metadata" .= LMap.delete "files" conArtifactMetadata
      ]

extractRevision :: OverrideProject -> ContainerScan -> ProjectRevision
extractRevision OverrideProject{..} ContainerScan{..} = ProjectRevision name revision Nothing
  where
    name = fromMaybe imageTag overrideName
    revision = fromMaybe imageDigest overrideRevision

toContainerScan :: Has Diagnostics sig m => SyftResponse -> m ContainerScan
toContainerScan SyftResponse{..} = do
  newArts <- context "error while validating system artifacts" $ traverse convertArtifact responseArtifacts
  let image = ContainerImage newArts (distroName responseDistro) (distroVersion responseDistro) (targetLayers $ sourceTarget responseSource)
      target = sourceTarget responseSource
  tag <- context "error while extracting image tags" . extractTag $ targetTags target
  pure . ContainerScan image tag $ targetDigest target

convertArtifact :: Has Diagnostics sig m => ResponseArtifact -> m ContainerArtifact
convertArtifact ResponseArtifact{..} = do
  let errMsg = "No metadata for system package with name: " <> artifactName
  validMetadata <- fromMaybeText errMsg artifactMetadata
  pure
    ContainerArtifact
      { conArtifactName = artifactName
      , conArtifactVersion = artifactVersion
      , conArtifactType = artifactType
      , conArtifactLocations = artifactLocations
      , conArtifactPkgUrl = artifactPkgUrl
      , conArtifactMetadataType = artifactMetadataType
      , conArtifactMetadata = validMetadata
      }

extractTag :: Has Diagnostics sig m => [Text] -> m Text
extractTag tags = do
  firstTag <- fromMaybeText "No image tags found" $ listToMaybe tags
  tagTuple <- fromMaybeText "Image was not in the format name:tag" $ breakOnAndRemove ":" firstTag
  pure $ fst tagTuple

runSyft ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , MonadIO m
  ) =>
  ImageText ->
  m SyftResponse
runSyft image = runExecIO . withSyftBinary $ \syftBin -> do
  execJson @SyftResponse [reldir|.|] $ syftCommand syftBin image

-- Scope to all layers, to prevent odd "squashing" that syft does
-- Output to produce machine readable json that we can ingest
syftCommand :: BinaryPaths -> ImageText -> Command
syftCommand bin (ImageText image) =
  Command
    { cmdName = pack . toFilePath $ toExecutablePath bin
    , cmdArgs = ["--scope", "all-layers", "-o", "json", image]
    , cmdAllowErr = Never
    }

parseSyftOutputMain :: Severity -> FilePath -> IO ()
parseSyftOutputMain logseverity path = withDefaultLogger logseverity . logWithExit_ . runReadFSIO $ parseSyftOutput path

parseSyftOutput :: (Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => FilePath -> m ()
parseSyftOutput filepath = do
  curdir <- sendIO getCurrentDir
  logDebug "Resolving file"
  path <- resolveFile curdir $ pack filepath
  logDebug "Reading JSON file"
  rawvalue <- readContentsJson @Value path
  logDebug "Parsing JSON contents"
  response <- fromEitherShow $ parseEither (parseJSON @SyftResponse) rawvalue
  logDebug "Converting to FOSSA payload"
  payload <- toContainerScan response
  logInfo "Payload is valid!"

  logStdout . decodeUtf8 $ encode payload

  pure ()

dumpSyftScanMain :: Severity -> Maybe FilePath -> ImageText -> IO ()
dumpSyftScanMain logseverity path image = withDefaultLogger logseverity . logWithExit_ . runExecIO $ dumpSyftScan path image

dumpSyftScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , MonadIO m
  ) =>
  Maybe FilePath ->
  ImageText ->
  m ()
dumpSyftScan path image = withSyftBinary $ \syft -> do
  syftOutput <- execThrow [reldir|.|] $ syftCommand syft image
  let writer :: BL.ByteString -> IO ()
      writer = maybe BL.putStr BL.writeFile path
  sendIO $ writer syftOutput
