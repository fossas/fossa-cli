{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.PackageReference (
  buildGraph,
  buildGraphWithCPM,
  analyze',
  analyzeWithCPM,
  PackageReference (..),
  ItemGroup (..),
  Package (..),
) where

import Control.Applicative (optional, (<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepType (NuGetType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (ReadFS, readContentsXML)
import Graphing (Graphing)
import Graphing qualified
import Parse.XML (FromXML (..), attr, child, children)
import Path (Abs, File, Path)
import Types (
  DependencyResults (..),
  GraphBreadth (Partial),
 )

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' = analyzeWithCPM Map.empty

-- | Analyze a project file, resolving missing PackageReference versions from
-- a CPM (Central Package Management) version map built from Directory.Packages.props.
analyzeWithCPM :: (Has ReadFS sig m, Has Diagnostics sig m) => Map Text Text -> Path Abs File -> m DependencyResults
analyzeWithCPM versionMap file = do
  ref <- readContentsXML @PackageReference file
  graph <- context "Building dependency graph" $ pure (buildGraphWithCPM versionMap ref)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [file]
      }

newtype PackageReference = PackageReference
  { groups :: [ItemGroup]
  }
  deriving (Eq, Ord, Show)

newtype ItemGroup = ItemGroup
  { dependencies :: [Package]
  }
  deriving (Eq, Ord, Show)

data Package = Package
  { depID :: Text
  , depVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromXML PackageReference where
  parseElement el = PackageReference <$> children "ItemGroup" el

instance FromXML ItemGroup where
  parseElement el = ItemGroup <$> children "PackageReference" el

-- | A "PackageReference" xml tag
--
-- See: https://docs.microsoft.com/en-us/dotnet/core/project-sdk/msbuild-props#packagereference
-- See: https://cloud.google.com/functions/docs/writing/specifying-dependencies-dotnet
instance FromXML Package where
  parseElement el =
    Package
      <$> (attr "Include" el <|> attr "Update" el)
      <*> optional (attr "Version" el <|> child "Version" el)

buildGraph :: PackageReference -> Graphing Dependency
buildGraph = buildGraphWithCPM Map.empty

-- | Build a dependency graph, resolving missing versions from a CPM version map.
-- When a PackageReference has no Version attribute, the version is looked up
-- from the map (sourced from Directory.Packages.props).
buildGraphWithCPM :: Map Text Text -> PackageReference -> Graphing Dependency
buildGraphWithCPM versionMap project = Graphing.fromList (map toDependency direct)
  where
    direct = concatMap dependencies (groups project)
    toDependency Package{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = depID
        , dependencyVersion = fmap CEq (depVersion <|> Map.lookup (Text.toCaseFold depID) versionMap)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
