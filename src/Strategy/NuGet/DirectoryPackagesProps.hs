module Strategy.NuGet.DirectoryPackagesProps (
  DirectoryPackagesProps (..),
  PackageVersionEntry (..),
  findAndParse,
  buildVersionMap,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, warnOnErr)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Diag.Common (MissingDeepDeps (MissingDeepDeps))
import Effect.ReadFS (ReadFS, doesFileExist, readContentsXML, resolveFile')
import Parse.XML (FromXML (..), attr, children)
import Path (Abs, Dir, File, Path, parent, toFilePath)

-- | Represents a parsed Directory.Packages.props file.
-- See: https://learn.microsoft.com/en-us/nuget/consume-packages/central-package-management
newtype DirectoryPackagesProps = DirectoryPackagesProps
  { packageVersionGroups :: [PackageVersionGroup]
  }
  deriving (Eq, Ord, Show)

newtype PackageVersionGroup = PackageVersionGroup
  { packageVersions :: [PackageVersionEntry]
  }
  deriving (Eq, Ord, Show)

data PackageVersionEntry = PackageVersionEntry
  { pvName :: Text
  , pvVersion :: Text
  }
  deriving (Eq, Ord, Show)

instance FromXML DirectoryPackagesProps where
  parseElement el = DirectoryPackagesProps <$> children "ItemGroup" el

instance FromXML PackageVersionGroup where
  parseElement el = PackageVersionGroup <$> children "PackageVersion" el

instance FromXML PackageVersionEntry where
  parseElement el =
    PackageVersionEntry
      <$> (attr "Include" el <|> attr "Update" el)
      <*> (attr "Version" el)

-- | Build a map from package name to version from a parsed Directory.Packages.props.
buildVersionMap :: DirectoryPackagesProps -> Map Text Text
buildVersionMap props =
  Map.fromList
    . map (\pv -> (Text.toCaseFold (pvName pv), pvVersion pv))
    . concatMap packageVersions
    $ packageVersionGroups props

-- | Search for Directory.Packages.props starting from the given directory,
-- walking up parent directories. If found, parse it and return the version map.
findAndParse ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs Dir ->
  m (Map Text Text)
findAndParse dir = warnOnErr MissingDeepDeps $ do
  found <- findPropsFile dir
  case found of
    Nothing -> pure Map.empty
    Just propsFile -> do
      props <- readContentsXML @DirectoryPackagesProps propsFile
      pure (buildVersionMap props)

-- | Walk up from @dir@ looking for Directory.Packages.props.
findPropsFile ::
  (Has ReadFS sig m) =>
  Path Abs Dir ->
  m (Maybe (Path Abs File))
findPropsFile dir = do
  let parentDir = parent dir
  resolved <- resolveFile' dir "Directory.Packages.props"
  case resolved of
    Right file -> do
      exists <- doesFileExist file
      if exists
        then pure (Just file)
        else
          if toFilePath dir == toFilePath parentDir
            then pure Nothing -- reached root
            else findPropsFile parentDir
    Left _ ->
      if toFilePath dir == toFilePath parentDir
        then pure Nothing
        else findPropsFile parentDir
