module Strategy.NuGet.DirectoryPackagesProps (
  DirectoryPackagesProps (..),
  PackageVersionEntry (..),
  findAndParse,
  buildVersionMap,
) where

import Control.Applicative (optional, (<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, warnOnErr)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Diag.Common (MissingDeepDeps (MissingDeepDeps))
import Effect.ReadFS (ReadFS, doesFileExist, readContentsXML, resolveFile')
import Parse.XML (FromXML (..), attr, child, children)
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

-- | A single @\<PackageVersion\>@ item. MSBuild allows shapes beyond
-- @\<PackageVersion Include="..." Version="..." /\>@: the @Version@ metadata
-- may appear as a child element instead of an attribute, and items like
-- @\<PackageVersion Remove="..." /\>@ carry no version at all. Entries missing
-- a name or version are kept as 'Nothing' and skipped by 'buildVersionMap'
-- rather than failing the parse of the whole file.
data PackageVersionEntry = PackageVersionEntry
  { pvName :: Maybe Text
  , pvVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromXML DirectoryPackagesProps where
  parseElement el = DirectoryPackagesProps <$> children "ItemGroup" el

instance FromXML PackageVersionGroup where
  parseElement el = PackageVersionGroup <$> children "PackageVersion" el

instance FromXML PackageVersionEntry where
  parseElement el =
    PackageVersionEntry
      <$> optional (attr "Include" el <|> attr "Update" el)
      <*> optional (attr "Version" el <|> child "Version" el)

-- | Build a map from package name to version from a parsed Directory.Packages.props.
buildVersionMap :: DirectoryPackagesProps -> Map Text Text
buildVersionMap props =
  Map.fromList
    . concatMap (mapMaybe toPair . packageVersions)
    $ packageVersionGroups props
  where
    toPair pv = (,) . Text.toCaseFold <$> pvName pv <*> pvVersion pv

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
