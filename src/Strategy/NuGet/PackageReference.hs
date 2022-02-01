{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.PackageReference (
  discover,
  findProjects,
  getDeps,
  mkProject,
  buildGraph,
  PackageReference (..),
  PackageReferenceProject (..),
  ItemGroup (..),
  Package (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Applicative (optional, (<|>))
import Control.Effect.Diagnostics
import Data.Aeson (ToJSON)
import Data.Foldable (find)
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import GHC.Generics (Generic)
import Graphing (Graphing)
import Graphing qualified
import Parse.XML
import Path
import Types

isPackageRefFile :: Path b File -> Bool
isPackageRefFile file = any (\x -> x `L.isSuffixOf` fileName file) [".csproj", ".xproj", ".vbproj", ".dbproj", ".fsproj"]

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject PackageReferenceProject]
discover dir = context "PackageReference" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [PackageReferenceProject]
findProjects = walk' $ \_ _ files -> do
  case find isPackageRefFile files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([PackageReferenceProject file], WalkContinue)

newtype PackageReferenceProject = PackageReferenceProject
  { packageReferenceFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PackageReferenceProject

instance AnalyzeProject PackageReferenceProject where
  analyzeProject _ = getDeps

mkProject :: PackageReferenceProject -> DiscoveredProject PackageReferenceProject
mkProject project =
  DiscoveredProject
    { projectType = PackageReferenceProjectType
    , projectBuildTargets = mempty
    , projectPath = parent $ packageReferenceFile project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PackageReferenceProject -> m DependencyResults
getDeps = context "PackageReference" . context "Static analysis" . analyze' . packageReferenceFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  ref <- readContentsXML @PackageReference file
  graph <- context "Building dependency graph" $ pure (buildGraph ref)
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
    Package <$> attr "Include" el
      <*> optional (attr "Version" el <|> child "Version" el)

buildGraph :: PackageReference -> Graphing Dependency
buildGraph project = Graphing.fromList (map toDependency direct)
  where
    direct = concatMap dependencies (groups project)
    toDependency Package{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = depID
        , dependencyVersion = fmap CEq depVersion
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
