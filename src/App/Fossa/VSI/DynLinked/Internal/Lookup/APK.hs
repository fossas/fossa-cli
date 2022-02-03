{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module App.Fossa.VSI.DynLinked.Internal.Lookup.APK (
  lookupDependencies,
  constructLookupTables,
  tryLookup,
  SyftData (..),
  SyftArtifact (..),
  SyftArtifactMetadata (..),
  SyftArtifactMetadataFile (..),
  SyftLookupTable (..),
) where

import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import App.Fossa.VSI.DynLinked.Util (runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Monad (join)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Aeson (FromJSON, Result (Error, Success), ToJSON, Value, fromJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson)
import Path (Abs, Dir, File, Path)

-- | The idea here is that we look up what paths we can with apt and turn them into @DynamicDependency@.
-- We then hand back leftovers and lookup results for the next resolution function.
lookupDependencies :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> [Path Abs File] -> m ([Path Abs File], [DynamicDependency])
lookupDependencies _ files | not runningLinux = pure (files, [])
lookupDependencies root files = do
  syft <- runSyft root
  table <- constructLookupTables syft
  partitionEithers <$> traverse (tryLookup table) files

tryLookup :: (Has Diagnostics sig m) => SyftLookupTable -> Path Abs File -> m (Either (Path Abs File) DynamicDependency)
tryLookup SyftLookupTable{..} file = fmap (maybeToRight file) . runMaybeT $ do
  index <- MaybeT . pure $ Map.lookup file pathToIndex
  meta <- MaybeT . pure $ Map.lookup index indexToMeta
  pure . DynamicDependency file . Just $ ResolvedLinuxPackage LinuxPackageManagerRPM meta

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
data SyftLookupTable = SyftLookupTable
  { pathToIndex :: Map (Path Abs File) Word
  , indexToMeta :: Map Word LinuxPackageMetadata
  }
  deriving (Eq, Show)

constructLookupTables :: (Has Diagnostics sig m) => SyftData -> m SyftLookupTable
constructLookupTables SyftData{syftArtifacts} = do
  tables <- traverse construct $ zip [0 ..] syftArtifacts
  let (lookupTable, metadataTable) = flattenConstructed tables
  pure $ SyftLookupTable (Map.fromList lookupTable) (Map.fromList metadataTable)
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

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight df right = case right of
  Just a -> Right a
  Nothing -> Left df
