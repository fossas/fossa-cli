{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.Scan (
  runSyft,
  syftCommand,
  toContainerScan,
  extractTag,
  extractRevision,
  SyftResponse,
  ContainerScan (..),

  -- * Helpers
  LayerTarget (..),
) where

import App.Fossa.Config.Container.Common (ImageText (ImageText))
import App.Fossa.EmbeddedBinary (
  BinaryPaths,
  toPath,
  withSyftBinary,
 )
import App.Types (
  OverrideProject (..),
  ProjectRevision (ProjectRevision),
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  fromMaybeText,
 )
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  Value,
  object,
  withObject,
  (.:),
  (.:?),
 )
import Data.Functor.Extra ((<$$>))
import Data.Map (Map)
import Data.Map.Lazy qualified as LMap
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text.Extra (breakOnEndAndRemove)
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execJson,
 )
import Effect.ReadFS (ReadFS, getCurrentDir)

-- | The output of the syft binary
data SyftResponse = SyftResponse
  { responseArtifacts :: [ResponseArtifact]
  , responseSource :: ResponseSource
  , responseDistro :: ResponseDistro
  }

instance FromJSON SyftResponse where
  parseJSON = withObject "SyftResponse" $ \obj ->
    SyftResponse
      <$> (filter artifactTypeIsOk <$> obj .: "artifacts")
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
  , artifactLocations :: Set ContainerLocation
  , artifactPkgUrl :: Text
  , artifactMetadataType :: Text
  , artifactMetadata :: Maybe (Map Text Value)
  }

instance FromJSON ResponseArtifact where
  parseJSON = withObject "ResponseArtifact" $ \obj ->
    ResponseArtifact
      <$> obj .: "name"
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
    ResponseDistro
      <$> obj .: "name"
      <*> obj .: "version"

data SourceTarget = SourceTarget
  { targetDigest :: Text
  , targetLayers :: [LayerTarget]
  , targetTags :: [Text]
  }

instance FromJSON SourceTarget where
  parseJSON = withObject "SourceTarget" $ \obj ->
    SourceTarget
      <$> obj .: "imageID"
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
  deriving (Show, Eq)

instance ToJSON ContainerScan where
  toJSON scan = object ["image" .= imageData scan]

data ContainerImage = ContainerImage
  { imageArtifacts :: [ContainerArtifact]
  , imageOs :: Text
  , imageOsRelease :: Text
  , imageLayers :: [LayerTarget]
  }
  deriving (Show, Eq)

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
  , conArtifactLocations :: Set ContainerLocation
  , conArtifactPkgUrl :: Text
  , conArtifactMetadataType :: Text
  , conArtifactMetadata :: Map Text Value
  }
  deriving (Show, Eq)

instance ToJSON ContainerArtifact where
  toJSON ContainerArtifact{..} =
    object
      [ "name" .= conArtifactName
      , "fullVersion" .= conArtifactVersion
      , "type" .= conArtifactType
      , "locations" .= conArtifactLocations
      , "purl" .= conArtifactPkgUrl
      , "metadataType" .= conArtifactMetadataType
      , "metadata" .= LMap.delete "files" conArtifactMetadata
      ]

extractRevision :: OverrideProject -> ContainerScan -> ProjectRevision
extractRevision OverrideProject{..} ContainerScan{..} = ProjectRevision name revision overrideBranch
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
  tagTuple <- fromMaybeText "Image was not in the format name:tag" $ breakOnEndAndRemove ":" firstTag
  pure $ fst tagTuple

runSyft ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  ImageText ->
  m SyftResponse
runSyft image = withSyftBinary $ \syftBin -> do
  dir <- getCurrentDir
  execJson @SyftResponse dir $ syftCommand syftBin image

-- Scope to all layers, to prevent odd "squashing" that syft does
-- Output to produce machine readable json that we can ingest
syftCommand :: BinaryPaths -> ImageText -> Command
syftCommand bin (ImageText image) =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = ["--scope", "all-layers", "-o", "json", image]
    , cmdAllowErr = Never
    }
