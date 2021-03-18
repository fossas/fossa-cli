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
    parseSyftOutput,
    parseSyftOutputMain,
    dumpSyftScanMain,
  )
where

import App.Fossa.EmbeddedBinary (BinaryPaths, toExecutablePath, withSyftBinary)
import App.Types (ProjectRevision (..), OverrideProject (..))
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map.Lazy as LMap
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Extra (breakOnAndRemove)
import Effect.Exec (AllowErr (Never), Command (..), execJson, runExecIO, Exec, execThrow)
import Effect.Logger
import Effect.ReadFS (ReadFS, readContentsJson, ReadFSIOC (runReadFSIO), resolveFile) 
import Options.Applicative (Parser, argument, help, metavar, str)
import Path ( toFilePath, reldir, Dir, Rel )
import Path.IO (getCurrentDir)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Types (parseEither)
import qualified Data.Text.Lazy.Encoding as TE

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
    SyftResponse <$> (filter artifactTypeIsOk <$> obj .: "artifacts")
      <*> obj .: "source"
      <*> obj .: "distro"

artifactTypeIsOk :: ResponseArtifact -> Bool
artifactTypeIsOk art = artifactType art `elem` acceptedArtifactTypes

acceptedArtifactTypes :: [Text]
acceptedArtifactTypes = ["deb", "rpm", "apk"]

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

parseSyftOutputMain :: Severity -> FilePath -> IO ()
parseSyftOutputMain logseverity path = withLogger logseverity . logWithExit_ . runReadFSIO $ parseSyftOutput path

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

  logStdout . pretty . TE.decodeUtf8 $ encode payload

  pure ()

dumpSyftScanMain :: Severity -> Maybe FilePath -> ImageText -> IO ()
dumpSyftScanMain logseverity path image = withLogger logseverity . logWithExit_ . runExecIO $ dumpSyftScan path image

dumpSyftScan :: 
  ( Has Diagnostics sig m,
    Has (Lift IO) sig m,
    Has Exec sig m,
    MonadIO m
  ) =>
  Maybe FilePath ->
  ImageText ->
  m ()
dumpSyftScan path image = withSyftBinary $ \syft -> do
  syftOutput <- execThrow [reldir|.|] $ syftCommand syft image
  let writer :: BL.ByteString -> IO ()
      writer = maybe BL.putStr BL.writeFile path
  sendIO $ writer syftOutput