{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.Container
  ( ImageText (..),
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
  )
where

import App.Fossa.EmbeddedBinary (BinaryPaths, toExecutablePath, withSyftBinary)
import App.Types (ProjectRevision (..), OverrideProject (..))
import Control.Effect.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift (Lift)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map.Lazy as LMap
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Extra (breakOnAndRemove)
import Effect.Exec (AllowErr (Never), Command (..), execJson, runExecIO)
import Options.Applicative (Parser, argument, help, metavar, str)
import Path

newtype ImageText = ImageText {unImageText :: Text} deriving (Show, Eq, Ord)

imageTextArg :: Parser ImageText
imageTextArg = ImageText . pack <$> argument str (metavar "IMAGE" <> help "The image to scan")

newtype Locator = Locator {unLocator :: Text} deriving (Eq, Ord, Show)

-- | The output of the syft binary
data SyftResponse
  = SyftResponse
      { responseArtifacts :: [ResponseArtifact],
        responseSource :: ResponseSource,
        responseDistro :: ResponseDistro
      }

instance FromJSON SyftResponse where
  parseJSON = withObject "SyftResponse" $ \obj ->
    SyftResponse <$> obj .: "artifacts"
      <*> obj .: "source"
      <*> obj .: "distro"

data ResponseArtifact
  = ResponseArtifact
      { artifactName :: Text,
        artifactVersion :: Text,
        artifactType :: Text,
        artifactPkgUrl :: Text,
        artifactMetadataType :: Text,
        artifactMetadata :: Map Text Value
      }

instance FromJSON ResponseArtifact where
  parseJSON = withObject "ResponseArtifact" $ \obj ->
    ResponseArtifact <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .: "type"
      <*> obj .: "purl"
      <*> obj .: "metadataType"
      -- We delete "files" as early as possible, which reduces
      -- the size by over 95% in many cases.
      -- We use Lazy delete to avoid evaluating the innards of
      -- the field, since Aeson will try to avoid evaluating it
      -- as well.
      <*> (LMap.delete "files" <$> obj .: "metadata")

newtype ResponseSource
  = ResponseSource
      {sourceTarget :: SourceTarget}

instance FromJSON ResponseSource where
  parseJSON = withObject "ResponseSource" $ \obj ->
    ResponseSource <$> obj .: "target"

data ResponseDistro
  = ResponseDistro
      { distroName :: Text,
        distroVersion :: Text
      }

instance FromJSON ResponseDistro where
  parseJSON = withObject "ResponseDistro" $ \obj ->
    ResponseDistro <$> obj .: "name"
      <*> obj .: "version"

data SourceTarget
  = SourceTarget
      { targetDigest :: Text,
        targetTags :: [Text]
      }

instance FromJSON SourceTarget where
  parseJSON = withObject "SourceTarget" $ \obj ->
    SourceTarget <$> obj .: "digest"
      <*> obj .: "tags"

-- | The reorganized output of syft into a slightly different format
data ContainerScan
  = ContainerScan
      { imageData :: ContainerImage,
        imageTag :: Text,
        imageDigest :: Text
      }

instance ToJSON ContainerScan where
  toJSON scan = object ["image" .= imageData scan]

data ContainerImage
  = ContainerImage
      { imageArtifacts :: [ContainerArtifact],
        imageOs :: Text,
        imageOsRelease :: Text
      }

instance ToJSON ContainerImage where
  toJSON ContainerImage {..} =
    object
      [ "os" .= imageOs,
        "osRelease" .= imageOsRelease,
        "artifacts" .= imageArtifacts
      ]

data ContainerArtifact
  = ContainerArtifact
      { conArtifactName :: Text,
        conArtifactVersion :: Text,
        conArtifactType :: Text,
        conArtifactPkgUrl :: Text,
        conArtifactMetadataType :: Text,
        conArtifactMetadata :: Map Text Value
      }

instance ToJSON ContainerArtifact where
  toJSON ContainerArtifact {..} =
    object
      [ "name" .= conArtifactName,
        "fullVersion" .= conArtifactVersion,
        "type" .= conArtifactType,
        "purl" .= conArtifactPkgUrl,
        "metadataType" .= conArtifactMetadataType,
        "metadata" .= LMap.delete "files" conArtifactMetadata
      ]

extractRevision :: OverrideProject -> ContainerScan -> ProjectRevision
extractRevision OverrideProject {..} ContainerScan {..} = ProjectRevision name revision Nothing
  where
    name = fromMaybe imageTag overrideName
    revision = fromMaybe imageDigest overrideRevision


toContainerScan :: Has Diagnostics sig m => SyftResponse -> m ContainerScan
toContainerScan SyftResponse {..} = do
  let newArts = map convertArtifact responseArtifacts
      image = ContainerImage newArts (distroName responseDistro) (distroVersion responseDistro)
      target = sourceTarget responseSource
  tag <- extractTag $ targetTags target
  pure . ContainerScan image tag $ targetDigest target

convertArtifact :: ResponseArtifact -> ContainerArtifact
convertArtifact ResponseArtifact {..} = 
  ContainerArtifact
    { conArtifactName = artifactName,
      conArtifactVersion = artifactVersion,
      conArtifactType = artifactType,
      conArtifactPkgUrl = artifactPkgUrl,
      conArtifactMetadataType = artifactMetadataType,
      conArtifactMetadata = artifactMetadata
    }

extractTag :: Has Diagnostics sig m => [Text] -> m Text
extractTag tags = do
  firstTag <- fromMaybeText "No image tags found" $ listToMaybe tags
  tagTuple <- fromMaybeText "Image was not in the format name:tag" $ breakOnAndRemove ":" firstTag
  pure $ fst tagTuple

runSyft ::
  ( Has Diagnostics sig m,
    Has (Lift IO) sig m,
    MonadIO m
  ) =>
  ImageText ->
  m SyftResponse
runSyft image = runExecIO . withSyftBinary $ \syftBin -> do
  execJson @SyftResponse [reldir|.|] $ syftCommand syftBin image

syftCommand :: BinaryPaths -> ImageText -> Command
syftCommand bin (ImageText image) =
  Command
    { cmdName = pack . toFilePath $ toExecutablePath bin,
      cmdArgs = ["-o", "json", image],
      cmdAllowErr = Never
    }
