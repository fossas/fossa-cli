{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module App.Fossa.VSI.DynLinked.Internal.Lookup.APK (
  buildLookupTable,
  compileSyftOutput,
  apkTactic,
  SyftData (..),
  SyftArtifact (..),
  SyftArtifactMetadata (..),
  SyftArtifactMetadataFile (..),
  APKLookupTable (..),
) where

import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import App.Fossa.VSI.DynLinked.Util (runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Monad (join)
import Data.Aeson (FromJSON, Result (Error, Success), ToJSON, Value, fromJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson)
import Effect.Logger (Logger, logDebug, pretty)
import Path (Abs, Dir, File, Path)

apkTactic ::
  ( Has Logger sig m
  ) =>
  Maybe APKLookupTable ->
  Path Abs File ->
  m (Maybe DynamicDependency)
apkTactic (Just APKLookupTable{..}) file = do
  logDebug . pretty $ "[apk] resolving file: " <> show file
  case Map.lookup file pathToIndex of
    Nothing -> do
      logDebug "[apk] file not found in index"
      pure Nothing
    Just index -> do
      case Map.lookup index indexToMeta of
        Nothing -> do
          logDebug . pretty $ "[apk] package not found for index: " <> show index
          pure Nothing
        Just meta -> pure . Just <$> DynamicDependency file . Just $ ResolvedLinuxPackage LinuxPackageManagerRPM meta
apkTactic Nothing file = do
  logDebug . pretty $ "[apk] resolving file: " <> show file
  logDebug "[apk] no apk lookup table provided"
  pure Nothing

-- | The output of the syft binary
newtype SyftData = SyftData
  { syftArtifacts :: [SyftArtifact]
  }
  deriving (Eq, Show)

instance FromJSON SyftData where
  parseJSON = withObject "SyftData" $ \obj ->
    SyftData . filter isAPKArtifact <$> obj .: "artifacts"

data SyftArtifact = SyftArtifact
  { artifactName :: Text
  , artifactVersion :: Text
  , artifactType :: Text
  , artifactMetadataType :: Text
  , -- We may run this on systems containing packages whose type we don't handle.
    -- Delay parsing the metadata until it is required.
    artifactMetadata :: Value
  }
  deriving (Eq, Show)

instance FromJSON SyftArtifact where
  parseJSON = withObject "SyftArtifact" $ \obj ->
    SyftArtifact <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .: "type"
      <*> obj .: "metadataType"
      <*> obj .: "metadata"

data SyftArtifactMetadata = SyftArtifactMetadata
  { metadataArchitecture :: Text
  , metadataFiles :: [SyftArtifactMetadataFile]
  }
  deriving (Eq, Show)

instance FromJSON SyftArtifactMetadata where
  parseJSON = withObject "SyftArtifactMetadata" $ \obj ->
    SyftArtifactMetadata <$> obj .: "architecture"
      <*> obj .: "files"

instance ToJSON SyftArtifactMetadata where
  toJSON SyftArtifactMetadata{..} =
    object
      [ "architecture" .= metadataArchitecture
      , "files" .= metadataFiles
      ]

newtype SyftArtifactMetadataFile = SyftArtifactMetadataFile
  { metadataFilePath :: Path Abs File
  }
  deriving (Eq, Show)

instance FromJSON SyftArtifactMetadataFile where
  parseJSON = withObject "SyftArtifactMetadataFile" $ \obj ->
    SyftArtifactMetadataFile <$> obj .: "path"

instance ToJSON SyftArtifactMetadataFile where
  toJSON SyftArtifactMetadataFile{..} = object ["path" .= metadataFilePath]

isAPKArtifact :: SyftArtifact -> Bool
isAPKArtifact SyftArtifact{..} = artifactType == "apk" && artifactMetadataType == "ApkMetadata"

runSyft :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> m SyftData
runSyft root = execJson root syftCommand

syftCommand :: Command
syftCommand =
  Command
    { cmdName = "syft"
    , cmdArgs = ["/", "-o", "json"]
    , cmdAllowErr = Never
    }

-- | Multiple file paths can point to the same metadata.
data APKLookupTable = APKLookupTable
  { pathToIndex :: Map (Path Abs File) Word
  , indexToMeta :: Map Word LinuxPackageMetadata
  }
  deriving (Eq, Show)

buildLookupTable :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> m (Maybe APKLookupTable)
buildLookupTable root | runningLinux = do
  syft <- runSyft root
  Just <$> compileSyftOutput syft
buildLookupTable _ = pure Nothing

compileSyftOutput :: (Has Diagnostics sig m) => SyftData -> m APKLookupTable
compileSyftOutput SyftData{syftArtifacts} = do
  tables <- traverse construct $ zip [0 ..] syftArtifacts
  let (lookupTable, metadataTable) = flattenConstructed tables
  pure $ APKLookupTable (Map.fromList lookupTable) (Map.fromList metadataTable)
  where
    construct :: (Has Diagnostics sig m) => (Word, SyftArtifact) -> m ([(Path Abs File, Word)], (Word, LinuxPackageMetadata))
    construct (index, artifact@SyftArtifact{artifactMetadata}) = do
      syftMeta <- fatalParse $ fromJSON artifactMetadata
      pure (lookupTableEntries index syftMeta, fromSyft index artifact syftMeta)

    lookupTableEntries :: Word -> SyftArtifactMetadata -> [(Path Abs File, Word)]
    lookupTableEntries index = fmap ((,index) . metadataFilePath) . metadataFiles

    fromSyft :: Word -> SyftArtifact -> SyftArtifactMetadata -> (Word, LinuxPackageMetadata)
    fromSyft index artifact metadata =
      ( index
      , LinuxPackageMetadata
          { linuxPackageID = artifactName artifact
          , linuxPackageRevision = artifactVersion artifact
          , linuxPackageArch = metadataArchitecture metadata
          , linuxPackageDistroEpoch = Nothing
          }
      )

    flattenConstructed :: [([(a, b)], (b, c))] -> ([(a, b)], [(b, c)])
    flattenConstructed input = do
      let (left, right) = unzip input
      (join left, right)

fatalParse :: (Has Diagnostics sig m) => Result a -> m a
fatalParse r = case r of
  Error s -> fatalText $ toText s
  Success v -> pure v
